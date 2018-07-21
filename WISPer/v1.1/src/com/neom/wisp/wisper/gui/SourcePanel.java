package com.neom.wisp.wisper.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.plaf.basic.BasicPanelUI;

/**
 * @author Kevin Hunter
 *
 * Copyright (c) Shell Stream Software LLC, All Rights Reserved.
 */
public class SourcePanel extends JPanel
{
	public SourcePanel(JTextArea textArea)
	{
		super(new BorderLayout());
		
		m_editorFont = Main.getEditorFont();
		m_textArea = textArea;
		m_fontMetrics = textArea.getFontMetrics(m_editorFont);
		m_nCharWidth = m_fontMetrics.charWidth('M');
		m_nCharHeight = m_fontMetrics.getHeight();
		m_nCharBaseline = m_fontMetrics.getAscent();
		
		textArea.setFont(m_editorFont);
		textArea.setOpaque(false);
		//setBorder(new LineNumberBorder());
		setUI(new SourcePanelUI());
		
		add(new LineNumberArea(), BorderLayout.WEST);
		add(textArea, BorderLayout.CENTER);
	}
	
	public int getCharHeight()
	{
		return(m_nCharHeight);
	}
	
	public int getCharWidth()
	{
		return(m_nCharWidth);
	}
	
	private class LineNumberArea extends JPanel
	{
		public LineNumberArea()
		{
			setOpaque(false);
		}
		
		public Dimension getPreferredSize()
		{
			Dimension dim = new Dimension(m_nCharWidth * LINE_NUMBER_WIDTH, 10);
			return(dim);
		}
	}
	
	private class SourcePanelUI extends BasicPanelUI
	{
		public SourcePanelUI()
		{
		}
		
		public void paint(Graphics g, JComponent c)
		{
			int nHeight = c.getHeight();
			int nWidth = c.getWidth();
			
			Color oldColor = g.getColor();
			
			paintBackground(g, nWidth, nHeight);
			paintLineNumbers(g, nHeight);
			paintColumns(g, nWidth, nHeight);
			
			g.setColor(oldColor);
		}
		
		private void paintBackground(Graphics g, int nWidth, int nHeight)
		{
			g.setColor(Color.WHITE);
			g.fillRect(0, 0, nWidth, nHeight);
		}
		
		private void paintLineNumbers(Graphics g, int nHeight)
		{
			g.setColor(m_colorLineNumbers);
			g.fillRect(0, 0, m_nCharWidth*(LINE_NUMBER_WIDTH), nHeight);
			
			g.setFont(m_editorFont);
			g.setColor(Color.black);
			
			int nLines = m_textArea.getLineCount();
			int nStringY = m_nCharBaseline;
			int nStringX = 0;
			int nRightEdge = m_nCharWidth * (LINE_NUMBER_WIDTH-1);
			
			for (int i = 1; i <= nLines; i++)
			{
				String strLineNum = Integer.toString(i);
				
                nStringX = nRightEdge - m_fontMetrics.stringWidth(strLineNum);
                
                g.drawString(strLineNum, nStringX, nStringY);
                
                nStringY += m_nCharHeight;
			}
		}
		
		private void paintColumns(Graphics g, int nWidth, int nHeight)
		{
			g.setColor(m_colorColumns);
			
			g.fillRect(	m_nCharWidth*LINE_NUMBER_WIDTH, 0,
						m_nCharWidth * 6, nHeight);
						
			g.fillRect(	m_nCharWidth * (72+LINE_NUMBER_WIDTH), 0, 
						m_nCharWidth * 8, nHeight);
		}
	}
	
	private JTextArea		m_textArea;
	private Font			m_editorFont;
	private FontMetrics	m_fontMetrics;
	private int			m_nCharWidth;
	private int			m_nCharHeight;
	private int			m_nCharBaseline;
	
	private static final int LINE_NUMBER_WIDTH = 6;
	
	private static final Color m_colorColumns = new Color(230,230,230); // A little lighter gray then the line numbers
	private static final Color m_colorLineNumbers = Color.lightGray;
}
