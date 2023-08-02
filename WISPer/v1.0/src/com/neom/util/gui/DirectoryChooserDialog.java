package com.neom.util.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.io.File;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:     Shell Stream Software LLC
 * @author
 * @version 1.0
 */

public class DirectoryChooserDialog extends JDialog
{
    public DirectoryChooserDialog()
    {
        super((Frame)null, "Choose Directory", true);
        initialize();
    }

    public DirectoryChooserDialog(String strTitle)
    {
        super((Frame)null, strTitle, true);
        initialize();
    }

    public DirectoryChooserDialog(Dialog owner)
    {
        super(owner, true);
        initialize();
    }

    public DirectoryChooserDialog(Dialog owner, String strTitle)
    {
        super(owner, strTitle, true);
        initialize();
    }

    public DirectoryChooserDialog(Frame owner)
    {
        super(owner, true);
        initialize();
    }

    public DirectoryChooserDialog(Frame owner, String strTitle)
    {
        super(owner, strTitle, true);
        initialize();
    }

    private void initialize()
    {
        setName("DirectoryChooserDialog");

        JPanel panel = new JPanel();
        m_actionOk = new OkAction();
        m_actionCancel = new CancelAction();

        JButton buttonOk = new JButton(m_actionOk);
        JButton buttonCancel = new JButton(m_actionCancel);

        m_labelDirectory = new JLabel(" ");
        m_labelDirectory.setForeground(Color.black);

        panel.setLayout(new FlowLayout(FlowLayout.CENTER));
        panel.add(buttonOk);
        panel.add(buttonCancel);

        Container c = getContentPane();
        c.setLayout(new BorderLayout());

        m_tree = new DirectoryChooserTree();
        m_tree.addDirectoryChooserTreeListener(new TreeListener());
        m_tree.setDirectorySortCaseSensitive(m_bSortCaseSensitive);

        c.add(m_labelDirectory, BorderLayout.NORTH);
        c.add(new JScrollPane(m_tree), BorderLayout.CENTER);
        c.add(panel, BorderLayout.SOUTH);

        pack();
    }

    public File getDirectory()
    {
        return(m_directory);
    }

    public void setDirectory(File dir)
    {
        m_directory = dir;
    }

    public boolean showDialog()
    {
        prepareToShow();

        show();     // blocks

        return(m_bOkPressed);
    }

    protected void prepareToShow()
    {
        m_tree.setInitialDirectory(m_directory);
        validate();
        m_tree.scrollIfNecessary();
    }

    public boolean showDialog(File initialDirectory)
    {
        setDirectory(initialDirectory);
        return(showDialog());
    }

    public void setOkButtonLabel(String label)
    {
        m_actionOk.putValue(Action.NAME, label);
    }

    public void setCancelButtonLabel(String label)
    {
        m_actionCancel.putValue(Action.NAME, label);
    }

    /**
     *  Specifies the <code>Comparator</code> that will be used to sort
     *  lists of directories
     *  @param  c   The <code>Comparator</code> object.  Can be <code>null</code>
     */
    public void setDirectorySortCaseSensitive(boolean b)
    {
        m_bSortCaseSensitive = b;
        if (m_tree != null)
        {
            m_tree.setDirectorySortCaseSensitive(b);
        }
    }

    private File                    m_directory;
    private boolean                 m_bSortCaseSensitive = true;
    private DirectoryChooserTree    m_tree;
    private JLabel                  m_labelDirectory;
    private Action                  m_actionOk;
    private Action                  m_actionCancel;
    private boolean                 m_bOkPressed = false;
    private int                     m_nMinWidth = 200;
    private int                     m_nMinHeight = 200;

    private class OkAction extends AbstractAction
    {
        public OkAction()
        {
            super("OK");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e)
        {
            DirectoryChooserDialog.this.m_bOkPressed = true;
            DirectoryChooserDialog.this.hide();
        }
    }

    private class CancelAction extends AbstractAction
    {
        public CancelAction()
        {
            super("Cancel");
            setEnabled(true);
        }

        public void actionPerformed(ActionEvent e)
        {
            DirectoryChooserDialog.this.m_bOkPressed = false;
            DirectoryChooserDialog.this.hide();
        }
    }

    private class TreeListener implements DirectoryChooserTreeListener
    {
        public TreeListener()
        {
        }

        public void directorySelected(File dir)
        {
            if (dir == null)
            {
                DirectoryChooserDialog.this.m_actionOk.setEnabled(false);
                DirectoryChooserDialog.this.m_labelDirectory.setText(" ");
            }
            else
            {
                DirectoryChooserDialog.this.m_actionOk.setEnabled(true);
                DirectoryChooserDialog.this.m_labelDirectory.setText(dir.getAbsolutePath());
                DirectoryChooserDialog.this.m_directory = dir;
            }
        }
    }

    public static void main(String[] args)
    {
        File startingDirectory = null;
        String name = System.getProperty("user.home");
        //startingDirectory = new File(name);
        for(;;)
        {
            DirectoryChooserDialog chooser = new DirectoryChooserDialog();
            chooser.setTitle("Pick Directory");
            chooser.setOkButtonLabel("Pick");

            Dimension dimWindowSize = chooser.getSize();
            if (dimWindowSize.width < 300)
            {
                dimWindowSize.width = 300;
                chooser.setSize(dimWindowSize);
            }
            Dimension dimScreenSize = Toolkit.getDefaultToolkit().getScreenSize();

            int x = (dimScreenSize.width - dimWindowSize.width) / 2;
            int y = (dimScreenSize.height - dimWindowSize.height) / 2;

            chooser.setLocation(x,y);

            boolean bResult = chooser.showDialog(startingDirectory);
            if (!bResult)
            {
                System.exit(0);
            }

            startingDirectory = chooser.getDirectory();
        }
    }
}