package com.neom.util.gui;

import java.awt.FontMetrics;
import java.awt.Insets;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.border.AbstractBorder;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import java.awt.Rectangle;
import javax.swing.UIManager;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class LineNumberBorder extends AbstractBorder
{
    public LineNumberBorder()
    {
    }

    public void setNumberColor(Color c)
    {
        m_colorNumber = c;
    }

    public Color getNumberColor()
    {
        return(m_colorNumber);
    }

    public void setBackgroundColor(Color c)
    {
        m_colorBackground = c;
    }

    public Color getBackgroundColor()
    {
        return(m_colorBackground);
    }

    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height)
    {
        if (!(c instanceof JTextArea))
        {
            return;
        }

        FontData data = new FontData(c);

        Color oldColor = g.getColor();

        if (m_colorBackground != null)
        {
            g.setColor(m_colorBackground);
        }
        else
        {
            g.setColor(UIManager.getColor("Panel.background"));
        }

        g.fillRect(x, y, data.m_nBorderWidth, height);

        if (m_colorNumber != null)
        {
            g.setColor(m_colorNumber);
        }
        else
        {
            g.setColor(UIManager.getColor("Panel.foreground"));
        }

        FontMetrics fm = c.getFontMetrics(c.getFont());
        JTextArea textArea = (JTextArea)c;
        Insets insets = getBorderInsets(textArea);

        int nLines = textArea.getLineCount();
        for (int i = 0; i < nLines; i++)
        {
            try
            {
                String strLineNum = Integer.toString(i+1);

                /*
                 *  Find out where the i'th line starts within the document,
                 *  and convert that to an on-screen rectangle.  This gives
                 *  us the "y" value for the top of the text line
                 */
                int nLineOffset = textArea.getLineStartOffset(i);
                Rectangle r = textArea.modelToView(nLineOffset);

                /*
                 *  The insets give us how far the JTextArea text gets pushed in.
                 *  We back up from that to figure out where to start painting
                 *  the text.
                 */
                int nStringX = insets.left - fm.stringWidth(strLineNum) - data.m_nSpaceWidth;
                int nStringY = r.y + data.m_nVerticalOffset;
                g.drawString(strLineNum, nStringX, nStringY);
            }
            catch(BadLocationException e)
            {
                break;
            }
        }

        g.setColor(oldColor);
    }

    public boolean isBorderOpaque()
    {
        return(true);
    }

    public Insets getBorderInsets(Component c)
    {
        FontData data = new FontData(c);
        return(new Insets(0, data.m_nBorderWidth, 0, 0));
    }

    private class FontData
    {
        public FontData(Component c)
        {
            FontMetrics fm = c.getFontMetrics(c.getFont());
            m_nSpaceWidth = fm.stringWidth("X");
            m_nBorderWidth = fm.stringWidth("99999") + m_nSpaceWidth;
            m_nVerticalOffset = fm.getLeading() + fm.getMaxAscent();
        }

        public int m_nBorderWidth;      // total width of the left-hand border
        public int m_nVerticalOffset;   // amount we have to shift number vertically
                                        // to make it align properly
        public int m_nSpaceWidth;       // space to the right of the number
    }

    private Color   m_colorNumber;
    private Color   m_colorBackground;
}
