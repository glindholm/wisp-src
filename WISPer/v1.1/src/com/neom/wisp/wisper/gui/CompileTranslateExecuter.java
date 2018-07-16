package com.neom.wisp.wisper.gui;
import com.neom.wisp.wisper.*;
import java.io.*;
import javax.swing.*;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class CompileTranslateExecuter implements Runnable
{
	public static final int OPERATION_TRANSLATE				= 0;
	public static final int OPERATION_COMPILE				= 1;
	public static final int OPERATION_TRANSLATE_AND_COMPILE	= 2;

	public CompileTranslateExecuter(SourceModule[] moduleList, DialogProgress dlg, int nOperation)
	{
		m_moduleList = moduleList;
		m_dlgProgress = dlg;
		m_nOperation = nOperation;
	}
	
    public void execute()
    {
        new Thread(this).start();
        m_dlgProgress.show();       // blocks
    }

    public void run()
    {
        try
        {
        	if (m_nOperation == OPERATION_TRANSLATE_AND_COMPILE)
        	{
        		m_dlgProgress.setMaximum(m_moduleList.length * 2);
        	}
        	else
        	{
        		m_dlgProgress.setMaximum(m_moduleList.length);
        	}
        	
        	for (int i = 0; i < m_moduleList.length; i++)
        	{
            	runInternal(m_moduleList[i]);
            	
				if (m_dlgProgress.wasCancelled())
				{
					break;
				}
        	}
        }
        catch(Exception e)
        {
        	e.printStackTrace(System.err);
        }
        finally
        {
            m_dlgProgress.doHide();
        }
    }
    
    private void runInternal(SourceModule module)
    {
    	int nResult;
    	
		switch(m_nOperation)
		{
		case OPERATION_TRANSLATE:
			doTranslate(module);
			m_dlgProgress.tickCurrent();
			break;
			
		case OPERATION_COMPILE:
			doCompile(module);
			m_dlgProgress.tickCurrent();
			break;
		
		case OPERATION_TRANSLATE_AND_COMPILE:
		default:
			nResult = doTranslate(module);
			m_dlgProgress.tickCurrent();
			
			if (nResult == ProcessResult.FAILED)
			{
				m_dlgProgress.tickCurrent();
				break;
			}
			
			if (m_dlgProgress.wasCancelled())
			{
				break;
			}
			
			doCompile(module);
			m_dlgProgress.tickCurrent();
			break;
		}
    }

	public int doTranslate(SourceModule module)
	{
		if (!module.changeState(SourceModule.STATE_TRANSLATING))
		{
			return(ProcessResult.FAILED);
		}
		
	    ProcessResult result = module.getTranslationResult();
		m_dlgProgress.setLabelTranslating(module.getSourceFile().getName());
		
		module.clearStates();
		
    	DirectoryOptions options = UserSettings.getDirectoryOptions(module.getSourceDirectory());
    	ProgramSpec specTranslate = UserSettings.getTranslatorSpec();
    	if (!options.getUseGlobalTemplates())
    	{
    		specTranslate = new ProgramSpec(specTranslate.getExecutable(),
    										options.getTranslatorTemplate());
    	}
    	
    	int nRetval;
    	
    	if (!specTranslate.isValid())
    	{
    		nRetval = ProcessResult.FAILED;
    	}
    	else
    	{
	    	try
	    	{
	        	int nReturnCode = execute(module, specTranslate, result);
	    		result.finishProcessOutput();
	        	result.setStateTranslate(nReturnCode);
	    	}
	    	catch(ExecuteException e)
	    	{
	    		result.finishProcessOutput();
	    		result.setState(ProcessResult.FAILED);
	    	}
	    	
	    	nRetval = result.getState();
            SwingUtilities.invokeLater(new TranslatedProxy(module));
    	}
        
		module.changeState(SourceModule.STATE_IDLE);
        return(nRetval);
	}
	
	public int doCompile(SourceModule module)
	{
		if (!module.changeState(SourceModule.STATE_TRANSLATING))
		{
			return(ProcessResult.FAILED);
		}
		
	    ProcessResult result = module.getCompileResult();
		m_dlgProgress.setLabelCompiling(module.getSourceFile().getName());

    	DirectoryOptions options = UserSettings.getDirectoryOptions(module.getSourceDirectory());
    	ProgramSpec specCompile = UserSettings.getCompilerSpec();
    	if (!options.getUseGlobalTemplates())
    	{
    		specCompile = new ProgramSpec(specCompile.getExecutable(),
    										options.getCompilerTemplate());
    	}
    	
    	int nRetval;
    	
    	if (!specCompile.isValid())
    	{
    		nRetval = ProcessResult.FAILED;
    	}
    	else
    	{
	    	try
	    	{
	        	execute(module, specCompile, result);
	    		result.finishProcessOutput();
	        	result.setStateCompile();
	    	}
	    	catch(ExecuteException e)
	    	{
	    		result.finishProcessOutput();
	    		result.setState(ProcessResult.FAILED);
	    	}
	    	
	    	nRetval = result.getState();
            SwingUtilities.invokeLater(new CompiledProxy(module));
    	}
        
		module.changeState(SourceModule.STATE_IDLE);
        return(nRetval);
	}
	
	private int execute(SourceModule module, ProgramSpec spec, ProcessResult result) throws ExecuteException
	{
    	String strCommandLine = spec.buildCommand(module);
    	result.setCommandLine(strCommandLine);
    	result.beginProcessOutput();
    	int nReturnCode = 0;
    	
    	try
    	{
	        Process theProcess = null;
	        theProcess = Runtime.getRuntime().exec(strCommandLine, null, module.getSourceDirectory());
	        StreamDrainer stdoutDrainer = new StreamDrainer(theProcess.getInputStream(), result);
	        stdoutDrainer.start();
	        StreamDrainer stderrDrainer = new StreamDrainer(theProcess.getErrorStream(), result);
	        stderrDrainer.start();
	        nReturnCode = theProcess.waitFor();
	        stdoutDrainer.join();
	        stderrDrainer.join();
	 	}
        catch(IOException e)
        {
            result.abort("***IOException***");
            throw new ExecuteException();
        }
        catch(InterruptedException e)
        {
            result.abort("***Interrupted***");
            throw new ExecuteException();
        }
        
        return(nReturnCode);
	}
	
	private SourceModule[]	m_moduleList;
	private int			m_nOperation;
	private DialogProgress	m_dlgProgress;	
	
    private static class StreamDrainer extends Thread
    {
        public StreamDrainer(InputStream is, ProcessResult result)
        {
            m_is = is;
            m_result = result;
        }

        public void run()
        {
            try
            {
                for(;;)
                {
                    int nCount = m_is.read(m_buffer);
                    if (nCount < 0)
                    {
                        break;
                    }
                    if (nCount > 0)
                    {
						m_result.addToProcessOutput(m_buffer, 0, nCount);
                    }
                }
            }
            catch(IOException e)
            {
            }
        }

        private InputStream	m_is;
        private ProcessResult	m_result;
        private byte[]		m_buffer = new byte[4096];
    }
    
    private static class TranslatedProxy implements Runnable
    {
        public TranslatedProxy(SourceModule module)
        {
        	m_module = module;
        }

        public void run()
        {
            m_module.fireModuleTranslated();
        }
        
        private SourceModule m_module;
    }
    
    private static class CompiledProxy implements Runnable
    {
        public CompiledProxy(SourceModule module)
        {
        	m_module = module;
        }

        public void run()
        {
            m_module.fireModuleTranslated();
        }
        
        private SourceModule m_module;
    }
    
    private class ExecuteException extends Exception
    {
    }
}
