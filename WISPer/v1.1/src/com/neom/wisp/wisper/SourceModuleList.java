package com.neom.wisp.wisper;

import com.neom.wisp.wisper.gui.*;
import java.util.*;
import java.io.*;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class SourceModuleList implements FilenameFilter
{
	public SourceModuleList()
	{
	}
	
	public File getCurrentDirectory()
	{
		return(m_fileCurrentDirectory);
	}
	
	public void setCurrentDirectory(File directory)
	{
		m_fileCurrentDirectory = directory;
		
		purge();
		
		m_fileCurrentModules = directory.listFiles(this);
		if (m_fileCurrentModules == null)
		{
			m_fileCurrentModules = new File[0];
		}
		
		for (int i = 0; i < m_fileCurrentModules.length; i++)
		{
			add(new SourceModule(m_fileCurrentModules[i]));
		}
	}
	
	public File[] getCurrentModuleFiles()
	{
		return(m_fileCurrentModules);
	}
	
	public SourceModule getModule(File f)
	{
		if (f == null)
		{
			return(null);
		}
		
		SourceModule sm = null;
		try
		{
			sm = (SourceModule)m_modules.get(f.getCanonicalPath());
		}
		catch(IOException e)
		{
			sm = null;
		}
		
		return(sm);
	}
	
	public SourceModule[] getModules()
	{
		return((SourceModule[])m_modules.values().toArray(new SourceModule[m_modules.size()]));
	}
	
	public void purge()
	{
		if (m_fileCurrentDirectory == null)
		{
			return;
		}
		
		String strCurDirPath;
		
		try
		{
			strCurDirPath = m_fileCurrentDirectory.getCanonicalPath();
		}
		catch(IOException e)
		{
			return;
		}
		
		SourceModule[] modules = getModules();
		m_modules.clear();
		
		for (int i = 0; i < modules.length; i++)
		{
			if (modules[i].getSourceWindow() != null)
			{
				add(modules[i]);
			}
			else
			{
				try
				{
					String moduleDir = modules[i].getSourceDirectory().getCanonicalPath();
					if (moduleDir.equals(strCurDirPath))
					{
						add(modules[i]);
					}
				}
				catch(IOException e)
				{
					add(modules[i]);
				}
			}
		}
	}
	
	public void fireSettingsChanged()
	{
		Iterator iter = m_modules.values().iterator();
		while(iter.hasNext())
		{
			SourceModule sm = (SourceModule)iter.next();
			
			SourceWindow win = sm.getSourceWindow();
			if (win != null)
			{
				win.settingsChanged();
			}
		}
	}
	
	public boolean accept(File dir, String name)
	{
		if (name == null)
		{
			return(false);
		}
		
		int len = name.length();
		if (len < 5)
		{
			return(false);
		}
		
		String extension = name.substring(len - 4, len).toLowerCase();
		return(extension.equals(".wcb"));
	}
	
	private void add(SourceModule sm)
	{
		try
		{
			m_modules.put(sm.getSourceFile().getCanonicalPath(), sm);
		}
		catch(IOException e)
		{
		}
	}
	
	private File		m_fileCurrentDirectory;
	private File[]		m_fileCurrentModules;
	private HashMap	m_modules = new HashMap();
}
