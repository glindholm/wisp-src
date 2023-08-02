package com.neom.wisp.wisper;

import javax.swing.JPanel;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import javax.swing.JOptionPane;
import java.text.MessageFormat;
import javax.swing.JTabbedPane;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;
import java.awt.CardLayout;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.Font;
import com.neom.util.gui.LineNumberBorder;
import javax.swing.UIManager;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class FilePane extends JPanel
{
    public FilePane(MainWnd parent)
    {
        super(new BorderLayout());

        m_parent = parent;
        m_fontMono = new Font("Monospaced", Font.PLAIN, 12);
        m_splitter.setResizeWeight(1.0);

        add(m_splitter);

        int nPos = UserSettings.getHorizontalSplitterPos();
        if (nPos > 0)
        {
            m_splitter.setDividerLocation(nPos);
        }
    }

    public int getDividerLocation()
    {
        return(m_splitter.getDividerLocation());
    }

    public void setSelectedFile(FileListEntry file)
    {
        m_currentFile = file;
        int nDividerLocation = m_splitter.getDividerLocation();
        m_docWcbFile = null;
        m_docCobolFile = null;
        m_docWispError = null;
        m_docCobolError = null;

        if (file == null)
        {
            m_tabber = null;
            m_cardLayout = null;
            m_cardPanel = null;
            m_splitter.setTopComponent(new JPanel());
            m_splitter.setBottomComponent(new JPanel());
            m_splitter.setDividerLocation(nDividerLocation);
            m_splitter.invalidate();
            m_splitter.repaint();
            return;
        }

        if (!loadWispFileDocument(file))
        {
            showCantReadError(file.getWcbFileName());
            return;
        }

        if (!loadCobolFileDocument(file))
        {
            showCantReadError(file.getCobolFileName());
        }

        if (!loadWispErrorDocument(file))
        {
            showCantReadError(file.getWispErrorFileName());
        }

        if (!loadCobolErrorDocument(file))
        {
            showCantReadError(file.getCobolErrorFileName());
        }

        m_tabber = new JTabbedPane();
        m_tabber.setOpaque(true);
        m_cardLayout = new CardLayout();
        m_cardPanel = new JPanel(m_cardLayout);

        JTextArea wcbFileText = new JTextArea(m_docWcbFile);
        wcbFileText.setEditable(false);
        wcbFileText.setFont(m_fontMono);
        wcbFileText.setBorder(new LineNumberBorder());
        m_tabber.addTab(file.getWcbFileName(), new JScrollPane(wcbFileText));

        if (m_docCobolFile != null)
        {
            JTextArea cobolFileText = new JTextArea(m_docCobolFile);
            cobolFileText.setEditable(false);
            cobolFileText.setFont(m_fontMono);
            cobolFileText.setBorder(new LineNumberBorder());
            m_tabber.addTab(file.getCobolFileName(), new JScrollPane(cobolFileText));
        }

        if (m_docWispError != null)
        {
            JTextArea wispErrorText = new JTextArea(m_docWispError);
            wispErrorText.setEditable(false);
            wispErrorText.setFont(m_fontMono);
            m_cardPanel.add(new JScrollPane(wispErrorText), WCB_CARD_NAME);
        }
        else
        {
            m_cardPanel.add(new JPanel(), WCB_CARD_NAME);
        }

        if (m_docCobolError != null)
        {
            JTextArea cobolErrorText = new JTextArea(m_docCobolError);
            cobolErrorText.setEditable(false);
            cobolErrorText.setFont(m_fontMono);
            m_cardPanel.add(new JScrollPane(cobolErrorText), COB_CARD_NAME);
        }
        else
        {
            m_cardPanel.add(new JPanel(), COB_CARD_NAME);
        }

        m_splitter.setTopComponent(m_tabber);
        m_splitter.setBottomComponent(m_cardPanel);
        m_splitter.setDividerLocation(nDividerLocation);

        m_tabber.addChangeListener(new TabListener());
    }

    private void showCantReadError(String file)
    {
        String[] input = new String[1];
        input[0] = file;
        String msg = MessageFormat.format(Text.getString("FilePane.error.CantReadFile"), input);
        JOptionPane.showMessageDialog(m_parent,
                                        msg,
                                        Text.getString("FilePane.title.Error"),
                                        JOptionPane.ERROR_MESSAGE);
    }

    private boolean loadFile(Document doc, File srcFile)
    {
        FileInputStream is = null;
        InputStreamReader sr = null;

        try
        {
            is = new FileInputStream(srcFile);
            sr = new InputStreamReader(is);
            for(;;)
            {
                int nRead = sr.read(m_inputBuffer);
                if (nRead < 0)
                {
                    break;
                }
                if (nRead > 0)
                {
                    doc.insertString(doc.getLength(), new String(m_inputBuffer, 0, nRead), null);
                }
            }
        }
        catch(IOException e)
        {
            return(false);
        }
        catch(BadLocationException e)
        {
            return(false);
        }
        finally
        {
            try
            {
                if (sr != null)
                {
                    sr.close();
                }
                if (is != null)
                {
                    is.close();
                }
            }
            catch(Exception e)
            {
            }
        }

        return(true);
    }

    private boolean loadWispFileDocument(FileListEntry item)
    {
        File theFile = item.getWcbFile();
        if (!theFile.exists())
        {
            return(false);
        }

        Document doc = new PlainDocument();
        if (!loadFile(doc, theFile))
        {
            return(false);
        }

        m_docWcbFile = doc;
        return(true);
    }

    private boolean loadWispErrorDocument(FileListEntry item)
    {
        File theFile = item.getWispErrorFile();
        if (!theFile.exists())
        {
            return(true);
        }

        Document doc = new PlainDocument();
        if (!loadFile(doc, theFile))
        {
            return(false);
        }

        m_docWispError = doc;
        return(true);
    }

    private boolean loadCobolFileDocument(FileListEntry item)
    {
        File theFile = item.getCobolFile();
        if (!theFile.exists())
        {
            return(true);
        }

        Document doc = new PlainDocument();
        if (!loadFile(doc, theFile))
        {
            return(false);
        }

        m_docCobolFile = doc;
        return(true);
    }

    private boolean loadCobolErrorDocument(FileListEntry item)
    {
        File theFile = item.getCobolErrorFile();
        if (!theFile.exists())
        {
            return(true);
        }

        Document doc = new PlainDocument();
        if (!loadFile(doc, theFile))
        {
            return(false);
        }

        m_docCobolError = doc;
        return(true);
    }

    private class TabListener implements ChangeListener
    {
        public TabListener()
        {
        }

        public void stateChanged(ChangeEvent e)
        {
            if (m_currentFile == null ||
                m_tabber == null ||
                m_cardLayout == null ||
                m_cardPanel == null)
            {
                return;
            }

            int nIndex = FilePane.this.m_tabber.getSelectedIndex();
            switch(nIndex)
            {
            case 0:
                m_cardLayout.show(m_cardPanel, WCB_CARD_NAME);
                break;
            case 1:
                m_cardLayout.show(m_cardPanel, COB_CARD_NAME);
                break;
            default:
                break;
            }
        }
    }

    private FileListEntry       m_currentFile;
    private MainWnd             m_parent;
    private Document            m_docWcbFile;
    private Document            m_docCobolFile;
    private Document            m_docWispError;
    private Document            m_docCobolError;
    private JTabbedPane         m_tabber;
    private JPanel              m_cardPanel;
    private CardLayout          m_cardLayout;
    private Font                m_fontMono;
    private JSplitPane          m_splitter      = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
    private char[]              m_inputBuffer   = new char[4096];
    private static final String WCB_CARD_NAME = "wcb";
    private static final String COB_CARD_NAME = "cob";
}