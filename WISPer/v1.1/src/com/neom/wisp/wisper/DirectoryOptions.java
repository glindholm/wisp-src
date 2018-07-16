package com.neom.wisp.wisper;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class DirectoryOptions
{
	public DirectoryOptions()
	{
		this(true, null, null);
	}
	
	public DirectoryOptions(boolean bUseGlobalTemplates, String strTranslatorTemplate, String strCompilerTemplate)
	{
		m_bUseGlobalTemplates = bUseGlobalTemplates;
		m_strTranslatorTemplate = strTranslatorTemplate;
		m_strCompilerTemplate = strCompilerTemplate;
	}
	
	public DirectoryOptions(DirectoryOptions other)
	{
		m_bUseGlobalTemplates = other.m_bUseGlobalTemplates;
		m_strTranslatorTemplate = other.m_strTranslatorTemplate;
		m_strCompilerTemplate = other.m_strCompilerTemplate;
	}
	
	public boolean getUseGlobalTemplates()
	{
		return(m_bUseGlobalTemplates);
	}
	
	public void setUseGlobalTemplates(boolean bUseGlobalTemplates)
	{
		m_bUseGlobalTemplates = bUseGlobalTemplates;
	}
	
	public String getTranslatorTemplate()
	{
		return(m_strTranslatorTemplate);
	}
	
	public void setTranslatorTemplate(String strTranslatorTemplate)
	{
		m_strTranslatorTemplate = strTranslatorTemplate;
	}
	
	public String getCompilerTemplate()
	{
		return(m_strCompilerTemplate);
	}
	
	public void setCompilerTemplate(String strCompilerTemplate)
	{
		m_strCompilerTemplate = strCompilerTemplate;
	}
	
	private boolean	m_bUseGlobalTemplates;
	private String		m_strTranslatorTemplate;
	private String		m_strCompilerTemplate;
}
