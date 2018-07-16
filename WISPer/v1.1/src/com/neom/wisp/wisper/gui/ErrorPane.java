package com.neom.wisp.wisper.gui;

import javax.swing.*;
import javax.swing.text.*;
import java.io.*;
import com.neom.wisp.wisper.*;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class ErrorPane extends JTextPane
{
	private static final int		FontSize	= 12;
	private static final float	CmdIndent = 18.0F;
	private static final float	ErrorIndent = 18.0F;
	
	public ErrorPane()
	{
		m_styleContext = new StyleContext();
		
		m_styleBase = m_styleContext.new NamedStyle();
		StyleConstants.setFontFamily(m_styleBase, Main.getEditorFontFamily());
		StyleConstants.setFontSize(m_styleBase, Main.getEditorFontSize());
		StyleConstants.setLineSpacing(m_styleBase, 0.0F);
		
		m_styleCmd = m_styleContext.new NamedStyle(m_styleBase);
		StyleConstants.setLeftIndent(m_styleCmd, CmdIndent);
		
		m_styleErrors = m_styleContext.new NamedStyle(m_styleBase);
		StyleConstants.setLeftIndent(m_styleErrors, ErrorIndent);
		
		m_styleTitles = m_styleContext.new NamedStyle(m_styleBase);
		StyleConstants.setBold(m_styleTitles, true);
	}
	
	public void clearContents()
	{
		StyledDocument styledDoc = getStyledDocument();
		clear(styledDoc);
	}
	
	public void setContents(SourceModule sm)
	{
		StyledDocument styledDoc = getStyledDocument();
		clear(styledDoc);
		addContents(styledDoc, sm);
	}
	
	public void setContents(SourceModuleList list)
	{
		StyledDocument styledDoc = getStyledDocument();
		clear(styledDoc);
		File[] files = list.getCurrentModuleFiles();
		for (int i = 0; i < files.length; i++)
		{
			SourceModule sm = list.getModule(files[i]);
			addContents(styledDoc, sm);
		}
	}
	
	public void setContents(SourceModule[] list)
	{
		StyledDocument styledDoc = getStyledDocument();
		clear(styledDoc);
		for (int i = 0; i < list.length; i++)
		{
			addContents(styledDoc, list[i]);
		}
	}
	
	private void clear(StyledDocument styledDoc)
	{
		int nLength = styledDoc.getLength();
		try
		{
			styledDoc.remove(0, nLength);
		}
		catch(BadLocationException e)
		{
		}
	}
	
	private void addContents(StyledDocument styledDoc, SourceModule sm)
	{
		addResults(styledDoc, sm.getSourceFile().getName(), sm.getTranslationResult(), "ErrorPane.msg.Translate");
		addResults(styledDoc, sm.getTranslatedFile().getName(), sm.getCompileResult(), "ErrorPane.msg.Compile");
	}
	
	private void addResults(StyledDocument styledDoc, String strFileName, ProcessResult result, String strTitleKey)
	{
		if (result.getState() == ProcessResult.NOT_RUN)
		{
			return;
		}
		
		String[] fileNameInsert = { strFileName };
		addLine(styledDoc, Text.getMessage(strTitleKey, fileNameInsert), m_styleTitles);
		String[] outputLines = result.getOutputLines();
		if (outputLines == null || outputLines.length == 0)
		{
			return;
		}
		addLine(styledDoc, result.getCommandLine(), m_styleCmd);
		for (int i = 0; i < outputLines.length; i++)
		{
			addLine(styledDoc, outputLines[i], m_styleErrors);
		}
	}
	
	private void addLine(StyledDocument styledDoc, String strLine, Style style)
	{
		try
		{
			styledDoc.setLogicalStyle(styledDoc.getLength(), style);
			styledDoc.insertString(styledDoc.getLength(),
									strLine, 
									style);
			styledDoc.insertString(styledDoc.getLength(),
									"\n", 
									style);
		}
		catch(BadLocationException e)
		{
		}
	}
	
	private StyleContext				m_styleContext;
	private StyleContext.NamedStyle	m_styleBase;
	private StyleContext.NamedStyle	m_styleTitles;
	private StyleContext.NamedStyle	m_styleCmd;
	private StyleContext.NamedStyle	m_styleErrors;
}
