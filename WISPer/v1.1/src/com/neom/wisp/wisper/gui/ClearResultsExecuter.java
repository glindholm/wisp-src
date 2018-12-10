package com.neom.wisp.wisper.gui;
import com.neom.wisp.wisper.SourceModule;

/**
 * @author Kevin Hunter
 *
 * Copyright (c) Shell Stream Software LLC, All Rights Reserved.
 */
public class ClearResultsExecuter implements Runnable
{
	public ClearResultsExecuter(SourceModule[] moduleList, DialogProgress dlg)
	{
		m_moduleList = moduleList;
		m_dlgProgress = dlg;
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
	        m_dlgProgress.setMaximum(m_moduleList.length);
	        
	    	for (int i = 0; i < m_moduleList.length; i++)
	    	{
				if (m_dlgProgress.wasCancelled())
				{
					break;
				}
				
				m_dlgProgress.setLabelClearing(m_moduleList[i].getSourceFile().getName());
				
				m_moduleList[i].deleteProperties();
				
				m_dlgProgress.tickCurrent();
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

	private SourceModule[]	m_moduleList;
	private DialogProgress	m_dlgProgress;	
}
