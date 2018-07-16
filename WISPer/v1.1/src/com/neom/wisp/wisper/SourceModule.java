package com.neom.wisp.wisper;

import com.neom.wisp.wisper.gui.*;
import java.io.*;
import java.util.*;
import com.neom.util.*;
import org.w3c.dom.*;
import javax.xml.parsers.*;

/**
 * Represents a single source module, its state, and its
 * related files.  The input source module is expected to
 * have the extension ".wcb", although the case of the
 * extension could be varied, or even mixed.
 * 
 * @author khunter
 *
 * 
 */

public class SourceModule
{
	public static final int STATE_IDLE			= 0;
	public static final int STATE_TRANSLATING	= 1;
	public static final int STATE_COMPILING		= 2;
	
	public SourceModule(File theFile)
	{
		m_nState = STATE_IDLE;
		
		String fileName = theFile.getName();
		m_strFileBase = fileName.substring(0, fileName.length() - 4);
		
		m_fileSource = theFile;
		m_fileSourceDirectory = theFile.getParentFile();
		m_fileTranslated = new File(m_fileSourceDirectory, m_strFileBase + ".cob");
		
		m_filePropertiesDirectory = new File(m_fileSourceDirectory, ".wisper");
		m_fileProperties = new File(m_filePropertiesDirectory, m_strFileBase + ".wisper");
		
		m_resultsTranslate = new ProcessResult();
		m_resultsCompile = new ProcessResult();
		
		loadProperties();
	}
	
	public synchronized int getState()
	{
		return(m_nState);
	}
	
	public synchronized boolean changeState(int nNewState)
	{
		switch(m_nState)
		{
		default:
		case STATE_IDLE:
			break;
			
		case STATE_TRANSLATING:
		case STATE_COMPILING:
			if (nNewState != STATE_IDLE)
			{
				return(false);
			}
			break;
		}

		m_nState = nNewState;		
		return(true);
	}
	
	public String getFileBase()
	{
		return(m_strFileBase);
	}
	
	public File getSourceFile()
	{
		return(m_fileSource);
	}
	
	public File getSourceDirectory()
	{
		return(m_fileSourceDirectory);
	}
	
	public File getTranslatedFile()
	{
		return(m_fileTranslated);
	}
	
	public File getPropertiesDirectory()
	{
		return(m_filePropertiesDirectory);
	}
	
	public File getPropertiesFile()
	{
		return(m_fileProperties);
	}
	
	public void clearStates()
	{
		m_resultsTranslate.setState(ProcessResult.NOT_RUN);
		m_resultsCompile.setState(ProcessResult.NOT_RUN);
		fireModuleCleared();
	}
	
	public int getTranslationState()
	{
		return(m_resultsTranslate.getState());
	}
	
	public ProcessResult getTranslationResult()
	{
		return(m_resultsTranslate);
	}
	
	public int getCompileState()
	{
		return(m_resultsCompile.getState());
	}
	
	public ProcessResult getCompileResult()
	{
		return(m_resultsCompile);
	}
	
	public void addSourceModuleListener(SourceModuleListener listener)
	{
		if (m_listeners == null)
		{
			m_listeners = new HashSet();
		}
		
		m_listeners.add(listener);
	}
	
	public void removeSourceModuleListener(SourceModuleListener listener)
	{
		if (m_listeners != null)
		{
			m_listeners.remove(listener);
		}
	}
	
	public void fireModuleTranslated()
	{
		if (m_listeners != null)
		{
			Iterator iter = m_listeners.iterator();
			while(iter.hasNext())
			{
				SourceModuleListener listener = (SourceModuleListener)iter.next();
				listener.moduleTranslated();
			}
		}
	}
	
	public void fireModuleCompiled()
	{
		if (m_listeners != null)
		{
			Iterator iter = m_listeners.iterator();
			while(iter.hasNext())
			{
				SourceModuleListener listener = (SourceModuleListener)iter.next();
				listener.moduleCompiled();
			}
		}
	}
	
	public void fireModuleCleared()
	{
		if (m_listeners != null)
		{
			Iterator iter = m_listeners.iterator();
			while(iter.hasNext())
			{
				SourceModuleListener listener = (SourceModuleListener)iter.next();
				listener.moduleCleared();
			}
		}
	}
	
	public SourceWindow getSourceWindow()
	{
		return(m_sourceWindow);
	}
	
	public void setSourceWindow(SourceWindow w)
	{
		m_sourceWindow = w;
	}
	
	public void loadProperties()
	{
		Document doc = XmlUtil.parseDocumentSilent(m_fileProperties);
		if (doc == null)
		{
			return;
		}
		
		Element root = doc.getDocumentElement();
		
		Element eTranslate = XmlUtil.findChildElement(root, ElemTranslate);
		m_resultsTranslate.load(eTranslate);
		
		Element eCompile = XmlUtil.findChildElement(root, ElemCompile);
		m_resultsCompile.load(eCompile);
	}
	
	public boolean saveProperties()
	{
		if (!m_filePropertiesDirectory.exists())
		{
			if (!m_filePropertiesDirectory.mkdirs())
			{
				return(false);
			}
		}
		
		Document doc = null;
		try
		{
			doc = XmlUtil.createDocument(ElemRoot);
		}
		catch(ParserConfigurationException e)
		{
			return(false);
		}
		
		Element root = doc.getDocumentElement();
		
		Element eTranslate = XmlUtil.addChildElement(doc, root, ElemTranslate);
		m_resultsTranslate.save(doc, eTranslate);
		
		Element eCompile = XmlUtil.addChildElement(doc, root, ElemCompile);
		m_resultsCompile.save(doc, eCompile);
		
		return(XmlUtil.serializeDocumentSilent(doc, m_fileProperties));
	}
	
	public void deleteProperties()
	{
		m_fileProperties.delete();
		clearStates();
	}
	
	private int			m_nState;
	private String			m_strFileBase;
	private File			m_fileSource;
	private File			m_fileSourceDirectory;
	private File			m_fileTranslated;
	
	private File			m_filePropertiesDirectory;
	private File			m_fileProperties;
	
	private ProcessResult	m_resultsTranslate;
	private ProcessResult	m_resultsCompile;
	
	private HashSet		m_listeners;
	
	private SourceWindow	m_sourceWindow;
	
	private static final String ElemRoot			= "WISPer.status";
	private static final String ElemTranslate	= "translate";
	private static final String ElemCompile		= "compile";
	
}
