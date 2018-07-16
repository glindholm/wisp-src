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
 * Title:		DirectoryChooserDialog
 * Description:	A dialog that allows an existing directory to
 * 				be selected.
 * Copyright:   Copyright (c) 2002, 2003
 * Company:     NeoMedia Technologies, Inc.
 * @author		Kevin Hunter
 * @version	1.0
 */

public class DirectoryChooserDialog extends JDialog
{
	/**
	 * 	Default constructor
	 */
	
    public DirectoryChooserDialog()
    {
        super((Frame)null, "Choose Directory", true);
        initialize();
    }
    
    /**
     * 	Constructor allowing specification of the dialog title
     * 
     * @param strTitle	Title for the dialog
     */

    public DirectoryChooserDialog(String strTitle)
    {
        super((Frame)null, strTitle, true);
        initialize();
    }

    /**
     * 	Constructor for a DirectoryChooserDialog that is the
     * 	child of another dialog.
     * 
     * @param owner	Parent dialog
     */

    public DirectoryChooserDialog(Dialog owner)
    {
        super(owner, true);
        initialize();
    }

    /**
     * 	Constructor for a DirectoryChooserDialog that is the
     * 	child of another dialog.
     * 
     * @param owner	Parent dialog
     * @param strTitle	Title for the dialog
     */

    public DirectoryChooserDialog(Dialog owner, String strTitle)
    {
        super(owner, strTitle, true);
        initialize();
    }

    /**
     * 	Constructor for a DirectoryChooserDialog that is the
     * 	child of a frame window.
     * 
     * @param owner	Parent frame window
     */

    public DirectoryChooserDialog(Frame owner)
    {
        super(owner, true);
        initialize();
    }

    /**
     * 	Constructor for a DirectoryChooserDialog that is the
     * 	child of a frame window.
     * 
     * @param owner	Parent frame window
     * @param strTitle	Title for the dialog
     */

    public DirectoryChooserDialog(Frame owner, String strTitle)
    {
        super(owner, strTitle, true);
        initialize();
    }

	/**
	 * 	Get the directory chosen by the user, if the user presses
	 * 	the "OK" button.&nbsp;If the user presses "Cancel", this
	 * 	will return the value set by <code>setDirectory</code> or
	 * 	passed into <code>showDialog</code>.
	 * 
	 * @return	File object representing the chosen directory
	 * @see	com.neom.util.gui.DirectoryChooserDialog#setDirectory
	 * @see	com.neom.util.gui.DirectoryChooserDialog#showDialog(java.io.File)
	 */
	
    public File getDirectory()
    {
        return(m_directory);
    }

	/**
	 * 	Set the initial directory that should be selected in
	 * 	the dialog.
	 * 
	 * @param	dir	File object representing the directory to be
	 * 				initially selected.
	 * @see	com.neom.util.gui.DirectoryChooserDialog#getDirectory
	 */
	
    public void setDirectory(File dir)
    {
        m_directory = dir;
    }

	/**
	 * 	Display the dialog.&nbsp;This call blocks until the user
	 * 	presses the OK or Cancel button.
	 * 
	 * @return	<code>true</code> if the user pressed "OK", 
	 * 			<code>false</code> if the user pressed "Cancel"
	 */
	
    public boolean showDialog()
    {
        prepareToShow();

        show();     // blocks

        return(m_bOkPressed);
    }

	/**
	 * 	Display the dialog using the specified directory as the
	 * 	initially selected directory.&nbsp;This call blocks until
	 * 	the user presses the OK or Cancel button.
	 * 
	 * @param	initialDirectory	<code>File</code> object
	 * 				representing the directory to be initially
	 * 				selected.
	 * @return	<code>true</code> if the user pressed "OK", 
	 * 			<code>false</code> if the user pressed "Cancel"
	 */
	
    public boolean showDialog(File initialDirectory)
    {
        setDirectory(initialDirectory);
        return(showDialog());
    }

	/**
	 * 	Specify the string used for the "OK" button.
	 * 
	 * @param	label	<code>String</code> to be used instead of "OK"
	 */
	
    public void setOkButtonLabel(String label)
    {
        m_actionOk.putValue(Action.NAME, label);
    }

	/**
	 * 	Specify the string used for the "Cancel" button.
	 * 
	 * @param	label	<code>String</code> to be used instead of "Cancel"
	 */
	
    public void setCancelButtonLabel(String label)
    {
        m_actionCancel.putValue(Action.NAME, label);
    }

    /**
     *  Specifies whether or not the order of directory display is
     * 	case-sensitive.
     * 
     *  @param  bCaseSensitive	<code>true</code> if display should be
     * 				case-sensitive, <code>false</code> if it should be
     * 				case-insensitive.  Windows users typically expect
     * 				a case-insensitive sort order, while UNIX users
     * 				typically expect case-sensitive.
     */
    
    public void setDirectorySortCaseSensitive(boolean bCaseSensitive)
    {
        m_bSortCaseSensitive = bCaseSensitive;
        if (m_tree != null)
        {
            m_tree.setDirectorySortCaseSensitive(bCaseSensitive);
        }
    }

	/**
	 *	Prepares the dialog to be displayed by selecting the initial
	 * 	directory, sizing the dialog, and ensuring that the 
	 * 	initially-selected directory is visible in the dialog.
	 */
	
    protected void prepareToShow()
    {
        m_tree.setInitialDirectory(m_directory);
        validate();
        m_tree.scrollIfNecessary();
    }

	/*
	 *	This function initializes the contents of the dialog
	 */
	 
    private void initialize()
    {
        setName("DirectoryChooserDialog");

        JPanel buttonPanel = new JPanel();
        m_actionOk = new OkAction();
        m_actionCancel = new CancelAction();

        JButton buttonOk = new JButton(m_actionOk);
        JButton buttonCancel = new JButton(m_actionCancel);

        m_labelDirectory = new JLabel(" ");
        m_labelDirectory.setForeground(Color.black);

        buttonPanel.setLayout(new FlowLayout(FlowLayout.CENTER));
        buttonPanel.add(buttonOk);
        buttonPanel.add(buttonCancel);

        m_tree = new DirectoryChooserTree();
        m_tree.addDirectoryChooserTreeListener(new TreeListener());
        m_tree.setDirectorySortCaseSensitive(m_bSortCaseSensitive);

        Container c = getContentPane();
        c.setLayout(new BorderLayout());

        c.add(m_labelDirectory, BorderLayout.NORTH);
        c.add(new JScrollPane(m_tree), BorderLayout.CENTER);
        c.add(buttonPanel, BorderLayout.SOUTH);

        pack();
    }
    
    private File					m_directory;
    private boolean				m_bSortCaseSensitive = true;
    private DirectoryChooserTree	m_tree;
    private JLabel					m_labelDirectory;
    private Action					m_actionOk;
    private Action					m_actionCancel;
    private boolean				m_bOkPressed = false;
    private int					m_nMinWidth = 200;
    private int					m_nMinHeight = 200;

	/*
	 * A single instance of this class is used to respond to the "OK"
	 * button being pressed.  It sets "m_bOkPressed" to true, and then
	 * hides the dialog, causing the "show()" method to return.
	 */
	 
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

	/*
	 * A single instance of this class is used to respond to the "OK"
	 * button being pressed.  It sets "m_bOkPressed" to false, and then
	 * hides the dialog, causing the "show()" method to return.
	 */
	 
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

	/*
	 * A single instance of this class is used to listen to the
	 * DirectoryChooserTree, to receive notifications of changes in
	 * the selected directory.  When the selection changes, this
	 * listener makes the appropriate changes to:
	 * 		a) whether or not the "OK" button is enabled
	 * 		b) the contents of the text label at the top of the dialog
	 * 			containing the currently selected directory name
	 * 		c) the "m_directory" variable, containing the selected directory
	 */
	 
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