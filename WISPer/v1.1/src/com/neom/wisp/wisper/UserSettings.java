package com.neom.wisp.wisper;

import java.util.*;
import java.io.*;
import java.awt.*;
import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;
import org.w3c.dom.*;
import org.xml.sax.*;
import com.neom.util.*;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class UserSettings
{
    private static final String SettingsDir = ".WISPer";
    private static final String SettingsFile = "WISPerSettings.xml";

	private static final String ElemRoot					= "WISPer.settings";
    private static final String ElemMainWindow			= "mainWindow";
    private static final String ElemCurrentDirectory		= "currentDirectory";
    private static final String ElemCurrentDirectorySel	= "currentDirectorySel";
    private static final String ElemVerticalSplitterPos	= "verticalSplitterPos";
    private static final String ElemTranslator			= "translator";
    private static final String ElemCompiler				= "compiler";
    private static final String ElemExecuter				= "executer";
    private static final String ElemExecutable			= "executable";
    private static final String ElemArgumentTemplate		= "argumentTemplate";
    private static final String ElemDirectoryOptions		= "directoryOptions";
    private static final String ElemDirectory			= "directory";
    private static final String ElemCheckedFiles			= "checkedFiles";
    private static final String ElemFile					= "file";
    private static final String ElemOptions				= "options";
    private static final String ElemAllowCobEdit			= "allowCobEdit";

    private static final String AttrPath					= "path";
    private static final String ElemUseGlobalTemplates	= "useGlobalTemplates";
    private static final String ElemTranslatorTemplate	= "translatorTemplate";
    private static final String ElemCompilerTemplate		= "compilerTemplate";
    
    private static String       		m_strSettingsDirPath;
    private static String       		m_strSettingsFilePath;
    
    private static Rectangle			m_rectMainWnd;
    private static String				m_strCurrentDirectory;
    private static int				m_nVerticalSplitterPos;
    private static ProgramSpec		m_specTranslator;
    private static ProgramSpec		m_specCompiler;
    private static ProgramSpec		m_specExecuter;
    private static Map				m_mapDirectoryOptions;
    private static String[]			m_checkedFiles;
    private static boolean			m_bAllowCobEdit;

    static
    {
        StringBuffer buffer = new StringBuffer();
        buffer.append(System.getProperty("user.home"));
        buffer.append(File.separator);
        buffer.append(SettingsDir);
        m_strSettingsDirPath = buffer.toString();

        buffer.append(File.separator);
        buffer.append(SettingsFile);
        m_strSettingsFilePath = buffer.toString();

        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        
        m_rectMainWnd = new Rectangle();
        m_rectMainWnd.x = screenSize.width / 10;
        m_rectMainWnd.y = screenSize.height / 10;
        m_rectMainWnd.width = screenSize.width * 8 / 10;
        m_rectMainWnd.height = screenSize.height * 8 / 10;
        
        m_strCurrentDirectory = System.getProperty("user.home");
        m_nVerticalSplitterPos = m_rectMainWnd.width / 4;
        
        m_specTranslator = new ProgramSpec();
        m_specCompiler = new ProgramSpec();
        m_specExecuter = new ProgramSpec();
        
        m_mapDirectoryOptions = new HashMap();
        
        m_bAllowCobEdit = false;
    }
    
    public static void save()
    {
    	Document doc = null;
    	
    	try
    	{
    		doc = XmlUtil.createDocument(ElemRoot);
    	}
    	catch(ParserConfigurationException e)
    	{
    		return;
    	}
    	
    	Element root = doc.getDocumentElement();
    	
    	saveWindowRect(doc, root, ElemMainWindow, m_rectMainWnd);
    	XmlUtil.addChildElement(doc, root, ElemCurrentDirectory, m_strCurrentDirectory);
    	saveCheckedFiles(doc, root);
    	XmlUtil.addChildElement(doc, root, ElemVerticalSplitterPos, m_nVerticalSplitterPos);
    	saveProgramSpec(doc, root, ElemTranslator, m_specTranslator);
    	saveProgramSpec(doc, root, ElemCompiler, m_specCompiler);
    	saveProgramSpec(doc, root, ElemExecuter, m_specExecuter);
    	saveDirectoryOptions(doc, root, m_mapDirectoryOptions);
    	saveOptions(doc, root);
    	
        File theDir = new File(m_strSettingsDirPath);
        if (!theDir.exists())
        {
            if (!theDir.mkdir())
            {
                return;
            }
        }

		XmlUtil.serializeDocumentSilent(doc, m_strSettingsFilePath);
    }

    public static void load()
    {
    	Document doc = XmlUtil.parseDocumentSilent(m_strSettingsFilePath);
    	if (doc == null)
    	{
    		return;
    	}
    	
    	Element root = doc.getDocumentElement();
    	if (!ElemRoot.equals(root.getNodeName()))
    	{
    		return;
    	}
    	
    	loadWindowRect(m_rectMainWnd, XmlUtil.findChildElement(root, ElemMainWindow));
    	m_strCurrentDirectory = XmlUtil.getChildText(root, ElemCurrentDirectory, m_strCurrentDirectory);
    	loadCheckedFiles(root);
    	m_nVerticalSplitterPos = XmlUtil.getChildInteger(root, ElemVerticalSplitterPos, m_nVerticalSplitterPos);
    	loadProgramSpec(root, ElemTranslator, m_specTranslator, false);
    	loadProgramSpec(root, ElemCompiler, m_specCompiler, false);
    	loadProgramSpec(root, ElemExecuter, m_specExecuter, true);
    	loadDirectoryOptions(root, m_mapDirectoryOptions);
    	loadOptions(root);
    }
    
    private static void loadWindowRect(Rectangle dest, Element e)
    {
    	if (e == null)
    	{
    		return;
    	}
    	
    	dest.x = getIntegerAttribute(e, "x", dest.x);
    	dest.y = getIntegerAttribute(e, "y", dest.y);
    	dest.width = getIntegerAttribute(e, "width", dest.width);
    	dest.height = getIntegerAttribute(e, "height", dest.height);
    }
    
    private static void loadProgramSpec(Element parent, String strChildName, ProgramSpec spec, boolean bAllowNullExecutable)
    {
    	Element childElement = XmlUtil.findChildElement(parent, strChildName);
    	if (childElement == null)
    	{
    		return;
    	}
    	
    	String strExecutable = XmlUtil.getChildText(childElement, ElemExecutable, null);
    	String strArgTemplate = XmlUtil.getChildText(childElement, ElemArgumentTemplate, null);
		if (strArgTemplate == null)
		{
			strArgTemplate = "";
		}
		if (strExecutable == null)
		{
			if (!bAllowNullExecutable)
			{
				return;
			}
			strExecutable = "";
		}
		
		spec.setExecutable(strExecutable);
		spec.setArgumentTemplate(strArgTemplate);
    }
    
    private static void saveWindowRect(Document doc, Element parent, String strChildName, Rectangle rect)
    {
    	Element childElement = doc.createElement(strChildName);
    	
    	childElement.setAttribute("x", Integer.toString(rect.x));
    	childElement.setAttribute("y", Integer.toString(rect.y));
    	childElement.setAttribute("width", Integer.toString(rect.width));
    	childElement.setAttribute("height", Integer.toString(rect.height));

    	parent.appendChild(childElement);
    }
    
    private static void saveProgramSpec(Document doc, Element parent, String strChildName, ProgramSpec spec)
    {
    	Element childElement = doc.createElement(strChildName);
    	
    	XmlUtil.addChildElement(doc, childElement, ElemExecutable, spec.getExecutable());
    	XmlUtil.addChildElement(doc, childElement, ElemArgumentTemplate, spec.getArgumentTemplate());
    	
    	parent.appendChild(childElement);
    }
    
    private static int getIntegerAttribute(Element elem, String strAttrName, int defaultValue)
    {
    	String strAttrValue = elem.getAttribute(strAttrName);
    	if (strAttrValue == null)
    	{
    		return(defaultValue);
    	}
    	
    	try
    	{
    		return(Integer.parseInt(strAttrValue));
    	}
    	catch(NumberFormatException e)
    	{
    	}
    	
    	return(defaultValue);
    }
    
    private static void saveDirectoryOptions(Document doc, Element parent, Map mapDirectoryOptions)
    {
    	Element dirOptionsRoot = doc.createElement(ElemDirectoryOptions);
    	
    	Iterator dirIterator = mapDirectoryOptions.keySet().iterator();
    	while(dirIterator.hasNext())
    	{
    		String strPath = (String)dirIterator.next();
    		DirectoryOptions options = (DirectoryOptions)mapDirectoryOptions.get(strPath);
    		if (options != null)
    		{
    			Element dirElement = doc.createElement(ElemDirectory);
    			dirElement.setAttribute(AttrPath, strPath);
    			
    			XmlUtil.addChildElement(doc, dirElement, ElemUseGlobalTemplates, options.getUseGlobalTemplates());
    			XmlUtil.addChildElement(doc, dirElement, ElemTranslatorTemplate, options.getTranslatorTemplate());
    			XmlUtil.addChildElement(doc, dirElement, ElemCompilerTemplate, options.getCompilerTemplate());

				dirOptionsRoot.appendChild(dirElement);
    		}
    	}
    	
    	parent.appendChild(dirOptionsRoot);
    }

    private static void loadDirectoryOptions(Element root, Map mapDirectoryOptions)
    {
    	Element dirOptionsRoot = XmlUtil.findChildElement(root, ElemDirectoryOptions);
    	if (dirOptionsRoot == null)
    	{
    		return;
    	}
    	
    	mapDirectoryOptions.clear();
    	
    	Element directoryElement = XmlUtil.findChildElement(dirOptionsRoot, ElemDirectory);
    	while(directoryElement != null)
    	{
			String strPath = directoryElement.getAttribute(AttrPath);
			boolean bUseGlobal = XmlUtil.getChildBoolean(directoryElement, ElemUseGlobalTemplates, true);
			String strTranslatorTemplate = XmlUtil.getChildText(directoryElement, ElemTranslatorTemplate, null);
			String strCompilerTemplate = XmlUtil.getChildText(directoryElement, ElemCompilerTemplate, null);
			
			mapDirectoryOptions.put(strPath, new DirectoryOptions(bUseGlobal, strTranslatorTemplate, strCompilerTemplate));
    		
    		directoryElement = XmlUtil.findSiblingElement(directoryElement, ElemDirectory);
    	}
    }
    
    private static void saveCheckedFiles(Document doc, Element root)
    {
    	Element checkedFilesRoot = XmlUtil.addChildElement(doc, root, ElemCheckedFiles);
    	if (m_checkedFiles != null)
    	{
    		for (int i = 0; i < m_checkedFiles.length; i++)
    		{
    			XmlUtil.addChildElement(doc, checkedFilesRoot, ElemFile, m_checkedFiles[i]);
    		}
    	}
    }
    
    private static void loadCheckedFiles(Element root)
    {
    	ArrayList list = new ArrayList();
    	Element checkedFilesRoot = XmlUtil.findChildElement(root, ElemCheckedFiles);
    	if (checkedFilesRoot != null)
    	{
    		Element fileElement = XmlUtil.findChildElement(checkedFilesRoot, ElemFile);
    		while(fileElement != null)
    		{
    			list.add(XmlUtil.getChildText(fileElement, ""));
    			fileElement = XmlUtil.findSiblingElement(fileElement, ElemFile);
    		}
    	}
    	
    	m_checkedFiles = (String[])list.toArray(new String[list.size()]);
    }
    
    private static void saveOptions(Document doc, Element root)
    {
    	Element optionsRoot = XmlUtil.addChildElement(doc, root, ElemOptions);
    	XmlUtil.addChildElement(doc, optionsRoot, ElemAllowCobEdit, m_bAllowCobEdit);
    }
    
    private static void loadOptions(Element root)
    {
    	Element optionsRoot = XmlUtil.findChildElement(root, ElemOptions);
    	if (optionsRoot == null)
    	{
    		return;
    	}
    	
    	m_bAllowCobEdit = XmlUtil.getChildBoolean(optionsRoot, ElemAllowCobEdit, false);
    }

	public static Rectangle getMainWndRect()
	{
		return(m_rectMainWnd);
	}
	
    public static int getMainWndWidth()
    {
        return(m_rectMainWnd.width);
    }
    
    public static void setMainWndWidth(int n)
    {
        m_rectMainWnd.width = n;
    }

    public static int getMainWndHeight()
    {
        return(m_rectMainWnd.height);
    }
    
    public static void setMainWndHeight(int n)
    {
        m_rectMainWnd.height = n;
    }

    public static int getMainWndX()
    {
        return(m_rectMainWnd.x);
    }
    
    public static void setMainWndX(int n)
    {
        m_rectMainWnd.x = n;
    }

    public static int getMainWndY()
    {
        return(m_rectMainWnd.y);
    }
    
    public static void setMainWndY(int n)
    {
        m_rectMainWnd.y = n;
    }

    public static String getCurrentDirectory()
    {
        return(m_strCurrentDirectory);
    }
    
    public static void setCurrentDirectory(String value)
    {
        m_strCurrentDirectory = value;
    }

    public static String getCurrentDirectorySel()
    {
        return(null);
    }
    
    public static void setCurrentDirectorySel(String value)
    {
    }

    public static int getVerticalSplitterPos()
    {
        return(m_nVerticalSplitterPos);
    }
    
    public static void setVerticalSplitterPos(int n)
    {
        m_nVerticalSplitterPos = n;
    }
    
    public static ProgramSpec getTranslatorSpec()
    {
    	return(new ProgramSpec(m_specTranslator));
    }
    
    public static void setTranslatorSpec(ProgramSpec spec)
    {
    	m_specTranslator = new ProgramSpec(spec);
    }

    public static ProgramSpec getCompilerSpec()
    {
    	return(new ProgramSpec(m_specCompiler));
    }
    
    public static void setCompilerSpec(ProgramSpec spec)
    {
    	m_specCompiler = new ProgramSpec(spec);
    }
    
    public static ProgramSpec getExecuterSpec()
    {
    	return(new ProgramSpec(m_specExecuter));
    }
    
    public static void setExecuterSpec(ProgramSpec spec)
    {
    	m_specExecuter = new ProgramSpec(spec);
    }

    public static void setDirectoryOptions(String strPath, DirectoryOptions options)
    {
   		m_mapDirectoryOptions.put(strPath, new DirectoryOptions(options));
    }
    
    public static void setDirectoryOptions(File dir, DirectoryOptions options)
    {
    	setDirectoryOptions(dir.getAbsolutePath(), options);
    }
    
    public static DirectoryOptions getDirectoryOptions(File dir)
    {
    	return(getDirectoryOptions(dir.getAbsolutePath()));
    }

    public static DirectoryOptions getDirectoryOptions(String strPath)
    {
    	DirectoryOptions options = (DirectoryOptions)m_mapDirectoryOptions.get(strPath);
    	if (options == null)
    	{
    		options = new DirectoryOptions();
    	}
    	else
    	{
    		options = new DirectoryOptions(options);
    	}
    	
    	return(options);
    }
    
    public static void setCheckedFiles(String[] list)
    {
    	m_checkedFiles = list;
    }
    
    public static String[] getCheckedFiles()
    {
    	return(m_checkedFiles);
    }
    
    public static void setAllowCobEdit(boolean bAllowCobEdit)
    {
    	m_bAllowCobEdit = bAllowCobEdit;
    }
    
    public static boolean getAllowCobEdit()
    {
    	return(m_bAllowCobEdit);
    }
}