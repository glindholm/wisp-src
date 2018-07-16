package com.neom.wisp.wisper;

import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.InputStream;
import java.text.MessageFormat;
import javax.swing.JOptionPane;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class Operation implements Runnable
{
    public static final int TRANSLATE               = 0;
    public static final int COMPILE                 = 1;
    public static final int TRANSLATE_AND_COMPILE   = 2;

    public Operation(   Frame parent,
                        int nType,
                        FileListEntry[] entries,
                        Executables exes,
                        Arguments globalArgs,
                        Arguments dirArgs)
    {
        m_nType = nType;
        String strTitle = "";
        switch(nType)
        {
        case TRANSLATE:
            strTitle = Text.getString("DialogProgress.title.Translate");
            break;
        case COMPILE:
            strTitle = Text.getString("DialogProgress.title.Compile");
            break;
        case TRANSLATE_AND_COMPILE:
        default:
            strTitle = Text.getString("DialogProgress.title.TranslateCompile");
            break;
        }
        m_dlgProgress = new DialogProgress(parent, strTitle);
        m_entries = entries;
        m_exes = exes;
        m_globalArgs = globalArgs;
        m_dirArgs = dirArgs;
    }

    public void start()
    {
        new Thread(this).start();
        m_dlgProgress.show();       // blocks
    }

    public void run()
    {
        try
        {
            runInternal();
        }
        catch(ExecutionException e)
        {
            JOptionPane.showMessageDialog(  m_dlgProgress,
                                            e.getErrorMessage(),
                                            Text.getString("ExecutionException.title"),
                                            JOptionPane.ERROR_MESSAGE);
        }
        catch(Exception e)
        {
        }
        finally
        {
            m_dlgProgress.doHide();
        }
    }

    private String formatCommand(String executable, ArgumentTemplate args, FileListEntry entry)
    {
        StringBuffer buffer = new StringBuffer();
        if (executable.indexOf(' ') >= 0)
        {
            buffer.append('"');
            buffer.append(executable);
            buffer.append('"');
        }
        else
        {
            buffer.append(executable);
        }

        buffer.append(' ');
        buffer.append(args.substitute(entry));
        return(buffer.toString());
    }

    protected void runInternal() throws ExecutionException
    {
        int nCount = m_entries.length;
        if (m_nType == TRANSLATE_AND_COMPILE)
        {
            m_dlgProgress.setMaximum(nCount*2);
        }
        else
        {
            m_dlgProgress.setMaximum(nCount);
        }
        m_dlgProgress.setCurrent(0);

        for (int i = 0; i < nCount; i++)
        {
            FileListEntry entry = m_entries[i];
            try
            {
                if (m_nType == TRANSLATE || m_nType == TRANSLATE_AND_COMPILE)
                {
                    m_dlgProgress.setLabelTranslating(entry.getWcbFileName());
                    translate(entry);
                    if (m_dlgProgress.wasCancelled())
                    {
                        break;
                    }
                    m_dlgProgress.tickCurrent();
                }

                if (m_nType == TRANSLATE_AND_COMPILE)
                {
                    if (entry.getWispLight() == FileListEntry.RED_LIGHT)
                    {
                        m_dlgProgress.tickCurrent();
                        continue;
                    }
                }

                if (m_nType == COMPILE || m_nType == TRANSLATE_AND_COMPILE)
                {
                    m_dlgProgress.setLabelCompiling(entry.getCobolFileName());
                    compile(entry);
                    if (m_dlgProgress.wasCancelled())
                    {
                        break;
                    }
                    m_dlgProgress.tickCurrent();
                }
            }
            finally
            {
                entry.saveStatus();
            }
        }
    }

    private void translate(FileListEntry entry) throws ExecutionException
    {
        entry.getCobolErrorFile().delete();

        ArgumentTemplate args;
        if (m_dirArgs.getUseGlobal())
        {
            args = m_globalArgs.getTranslatorTemplate();
        }
        else
        {
            args = m_dirArgs.getTranslatorTemplate();
        }

        String strCommand = formatCommand(m_exes.getTranslator(), args, entry);

        entry.setWispLight(FileListEntry.RED_LIGHT);    // in case we blow up
        entry.setCobolLight(FileListEntry.NO_LIGHT);
        entry.setWispCommand(strCommand);
        entry.saveStatus();

        File fWorkingDir = entry.getDirectory();
        ExecutionResults results = execute(strCommand, fWorkingDir, entry.getWispErrorFile());

        if (results.m_nReturnCode != 0)
        {
            entry.setWispLight(FileListEntry.RED_LIGHT);
        }
        else
        {
            if (results.m_nByteCount == 0)
            {
                entry.setWispLight(FileListEntry.GREEN_LIGHT);
            }
            else
            {
                entry.setWispLight(FileListEntry.YELLOW_LIGHT);
            }
        }
    }

    private void compile(FileListEntry entry) throws ExecutionException
    {
        if (!entry.getCobolFile().exists())
        {
            return;
        }

        ArgumentTemplate args;
        if (m_dirArgs.getUseGlobal())
        {
            args = m_globalArgs.getCompilerTemplate();
        }
        else
        {
            args = m_dirArgs.getCompilerTemplate();
        }

        String strCommand = formatCommand(m_exes.getCompiler(), args, entry);

        entry.setCobolLight(FileListEntry.RED_LIGHT);    // in case we blow up
        entry.setCobolCommand(strCommand);
        entry.saveStatus();

        File fWorkingDir = entry.getDirectory();
        ExecutionResults results = execute(strCommand, fWorkingDir, entry.getCobolErrorFile());
        if (results.m_nByteCount == 0)
        {
            entry.setCobolLight(FileListEntry.GREEN_LIGHT);
        }
        else
        {
            entry.setCobolLight(FileListEntry.RED_LIGHT);
        }
    }

    private ExecutionResults execute(   String strCommand,
                                        File fWorkingDir,
                                        File fResults)
                                        throws ExecutionException
    {
        ExecutionResults results = new ExecutionResults();
        FileOutputStream os = null;
        Process theProcess = null;

        try
        {
            os = new FileOutputStream(fResults);
        }
        catch(FileNotFoundException e)
        {
            throw new ExecutionException(e, fResults);
        }

        try
        {
            theProcess = Runtime.getRuntime().exec(strCommand, null, fWorkingDir);
            StreamDrainer stdoutDrainer = new StreamDrainer(theProcess.getInputStream(), os);
            stdoutDrainer.start();
            StreamDrainer stderrDrainer = new StreamDrainer(theProcess.getErrorStream(), os);
            stderrDrainer.start();
            results.m_nReturnCode = theProcess.waitFor();
            stdoutDrainer.join();
            stderrDrainer.join();
            results.m_nByteCount = stdoutDrainer.getBytesTransferred() + stderrDrainer.getBytesTransferred();
        }
        catch(IOException e)
        {
            throw new ExecutionException(e);
        }
        catch(InterruptedException e)
        {
            throw new ExecutionException(e);
        }
        finally
        {
            if (os != null)
            {
                try
                {
                    os.close();
                }
                catch(Exception e)
                {
                }
            }
        }

        return(results);
    }

    private int                 m_nType;
    private DialogProgress      m_dlgProgress;
    private FileListEntry[]     m_entries;
    private Executables         m_exes;
    private Arguments           m_globalArgs;
    private Arguments           m_dirArgs;

    private static class ExecutionResults
    {
        public ExecutionResults()
        {
            m_nByteCount = -1;
            m_nReturnCode = -1;
        }

        public int  m_nByteCount;
        public int  m_nReturnCode;
    }

    private static class StreamDrainer extends Thread
    {
        public StreamDrainer(InputStream is, OutputStream os)
        {
            m_is = is;
            m_os = os;
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
                        synchronized(m_os)
                        {
                            m_os.write(m_buffer, 0, nCount);
                        }
                        m_nCount += nCount;
                    }
                }
            }
            catch(IOException e)
            {
            }
        }

        public int getBytesTransferred()
        {
            return(m_nCount);
        }

        private InputStream     m_is;
        private OutputStream    m_os;
        private byte[]          m_buffer = new byte[4096];
        private int             m_nCount = 0;
    }

    private class ExecutionException extends Exception
    {
        public ExecutionException(FileNotFoundException e, File f)
        {
            formatMessage("ExecutionException.msg.FileNotFoundException", f.getName());
        }

        public ExecutionException(IOException e)
        {
            formatMessage("ExecutionException.msg.IOException", e.toString());
        }

        public ExecutionException(InterruptedException e)
        {
            formatMessage("ExecutionException.msg.InterruptedException", e.toString());
        }

        public String getErrorMessage()
        {
            return(m_strErrorMessage);
        }

        private void formatMessage(String templateKey, String value)
        {
            String template = Text.getString(templateKey);
            String[] args = new String[1];
            args[0] = value;
            m_strErrorMessage = MessageFormat.format(template, args);
        }

        private String  m_strErrorMessage;
    }
}
