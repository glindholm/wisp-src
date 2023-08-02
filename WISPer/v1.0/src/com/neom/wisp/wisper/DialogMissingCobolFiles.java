package com.neom.wisp.wisper;

import javax.swing.JDialog;
import javax.swing.JButton;
import javax.swing.JTextArea;
import javax.swing.JLabel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import javax.swing.Box;
import javax.swing.JScrollPane;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.text.Document;
import javax.swing.text.BadLocationException;
import java.awt.Container;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogMissingCobolFiles extends JDialog
{
    public static boolean run(Frame parent, List files)
    {
        int nCount = files.size();
        if (nCount == 0)
        {
            return(true);
        }

        DialogMissingCobolFiles dlg = new DialogMissingCobolFiles(parent);
        for (int i = 0; i < nCount; i++)
        {
            dlg.addFile(files.get(i).toString());
        }

        dlg.pack();
        Main.setMinSize(dlg);
        Main.centerInScreen(dlg);
        dlg.show();

        return(dlg.m_bOkPressed);
    }

    public DialogMissingCobolFiles(Frame parent)
    {
        super(parent, Text.getString("DialogMissingCobolFiles.title"), true);
        setName("DialogMissingCobolFiles");

        Box b = Box.createVerticalBox();
        b.add(new JLabel(Text.getString("DialogMissingCobolFiles.text1")));
        b.add(new JLabel(Text.getString("DialogMissingCobolFiles.text2")));


        JPanel lowerPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
        lowerPanel.add(new JButton(new OkAction()));
        lowerPanel.add(new JButton(new CancelAction()));


        m_textArea = new JTextArea();
        m_textArea.setEditable(false);

        Container c = getContentPane();

        c.setLayout(new BorderLayout());
        c.add(b, BorderLayout.NORTH);
        c.add(new JScrollPane(m_textArea), BorderLayout.CENTER);
        c.add(lowerPanel, BorderLayout.SOUTH);
    }

    private void addFile(String name)
    {
        try
        {
            Document doc = m_textArea.getDocument();
            if (doc.getLength() > 0)
            {
                doc.insertString(doc.getLength(), System.getProperty("line.separator"), null);
            }
            doc.insertString(doc.getLength(), name, null);
        }
        catch(BadLocationException e)
        {
        }
    }

    private JTextArea   m_textArea;
    private boolean     m_bOkPressed = false;

    private class OkAction extends WispAction
    {
        public OkAction()
        {
            super("DialogMissingCobolFiles.button.Ok");
        }

        public void actionPerformed(ActionEvent e)
        {
            m_bOkPressed = true;
            DialogMissingCobolFiles.this.hide();
        }
    }

    private class CancelAction extends WispAction
    {
        public CancelAction()
        {
            super("DialogMissingCobolFiles.button.Cancel");
        }

        public void actionPerformed(ActionEvent e)
        {
            DialogMissingCobolFiles.this.hide();
        }
    }
}