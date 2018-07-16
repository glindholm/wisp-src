package com.neom.wisp.wisper;

import javax.swing.JOptionPane;
import java.util.ArrayList;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.Action;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.Dimension;
import java.awt.Point;
import java.io.File;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import java.awt.Component;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.AbstractAction;
import com.neom.util.gui.DirectoryChooserDialog;
import javax.swing.UIManager;
import java.text.MessageFormat;
import java.awt.Cursor;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class MainWnd extends JFrame
{
    public MainWnd()
    {
        setName("MainWnd");
        setIconImage(Images.getImage(Images.WISPerIconSmall));

        buildMenuBar();

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new MainWindowListener());

        String strCurrentDirectory = UserSettings.getCurrentDirectory();
        if (strCurrentDirectory == null)
        {
            strCurrentDirectory = System.getProperty("user.home");
        }
        m_currentDirectory = new File(strCurrentDirectory);

        m_splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, buildDirectoryPane(), buildFilePane());
        m_splitter.setResizeWeight(0.0);

        getContentPane().add(m_splitter, BorderLayout.CENTER);

        setLocation(UserSettings.getMainWndX(),
                    UserSettings.getMainWndY());
        setSize(UserSettings.getMainWndWidth(),
                UserSettings.getMainWndHeight());
        int nPos = UserSettings.getVerticalSplitterPos();
        if (nPos > 0)
        {
            m_splitter.setDividerLocation(nPos);
        }

        m_executables = UserSettings.getExecutables();
        m_globalArguments = UserSettings.getArguments();
        m_directoryArguments = new Arguments();

        Main.setMinSize(this);
        setDirectory(m_currentDirectory);
        m_fileList.restoreSelectionString(UserSettings.getCurrentDirectorySel());
    }

    private Component buildDirectoryPane()
    {
        m_fileList = new FileList(this);
        m_fileList.addFileListListener(new FileListSelectionListener());
        m_buttonChangeDirectoryAction.setCurrentDirectory(m_currentDirectory);
        m_dirChangeButton = new JButton(m_buttonChangeDirectoryAction);

        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());

        panel.add(m_dirChangeButton, BorderLayout.NORTH);
        JScrollPane scroller = new JScrollPane(m_fileList);
        panel.add(scroller, BorderLayout.CENTER);

        Box buttonPanel = Box.createHorizontalBox();
        for (int i = 0; i < m_selectMenu.length; i++)
        {
            buttonPanel.add(new JButton(m_selectMenu[i]));
        }
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return(panel);
    }

    private Component buildFilePane()
    {
        m_filePane = new FilePane(this);

        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());

        Box buttonPanel = Box.createHorizontalBox();
        for (int i = 0; i < m_selectMenu.length; i++)
        {
            buttonPanel.add(new JButton(m_operationMenu[i]));
        }
        panel.add(buttonPanel, BorderLayout.NORTH);
        panel.add(m_filePane, BorderLayout.CENTER);

        return(panel);
    }

    private void changeDirectory()
    {
        DirectoryChooserDialog dlg = new DirectoryChooserDialog(this);
        dlg.setDirectory(m_currentDirectory);
        dlg.setTitle(Text.getString("DirectoryChooserDialog.title"));
        dlg.setOkButtonLabel(Text.getString("DirectoryChooserDialog.ok"));
        dlg.setCancelButtonLabel(Text.getString("DirectoryChooserDialog.cancel"));
        dlg.setDirectorySortCaseSensitive(Main.isSystemCaseSensitive());
        Main.setMinSize(dlg);
        Main.centerInScreen(dlg);
        if (!dlg.showDialog())
        {
            return;
        }

        File theNewDirectory = dlg.getDirectory();
        if (theNewDirectory == null)
        {
            return;
        }
        if (m_currentDirectory != null && m_currentDirectory.equals(theNewDirectory))
        {
            return;
        }
        setDirectory(dlg.getDirectory());
    }

    public void setDirectory(File dir)
    {
        Cursor oldCursor = getCursor();
        setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );

        m_currentDirectory = dir;
        m_buttonChangeDirectoryAction.setCurrentDirectory(m_currentDirectory);
        m_fileList.setCurrentDirectory(m_currentDirectory);
        m_filePane.setSelectedFile(null);
        UserSettings.loadDirectoryArguments(m_currentDirectory,
                                            m_directoryArgumentFileName,
                                            m_directoryArguments);

        updateTitle();
        updateButtons();

        setCursor(oldCursor);
    }

    public void setSelectedFile(FileListEntry entry)
    {
        Cursor oldCursor = getCursor();
        setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );

        m_filePane.setSelectedFile(entry);

        setCursor(oldCursor);
    }

    private void updateTitle()
    {
        String template = Text.getString("MainWnd.title");
        String[] args = new String[1];
        args[0] = m_currentDirectory.getAbsolutePath();
        String title = MessageFormat.format(template, args);
        setTitle(title);
    }

    private void updateButtons()
    {
        updateFileListButtons();
        updateFilePaneButtons();
    }

    private void updateFileListButtons()
    {
        boolean bEnabled = true;
        if (m_fileList.getCount() == 0)
        {
            bEnabled = false;
        }

        for (int i = 0; i < m_selectMenu.length; i++)
        {
            m_selectMenu[i].setEnabled(bEnabled);
        }
    }

    private void updateFilePaneButtons()
    {
        boolean bEnabled = true;
        if (m_fileList.getCheckedCount() == 0)
        {
            bEnabled = false;
        }

        for (int i = 0; i < m_operationMenu.length; i++)
        {
            m_operationMenu[i].setEnabled(bEnabled);
        }
    }

    private void shutdown()
    {
        Dimension size = getSize();
        UserSettings.setMainWndWidth(size.width);
        UserSettings.setMainWndHeight(size.height);

        Point location = getLocation();
        UserSettings.setMainWndX(location.x);
        UserSettings.setMainWndY(location.y);

        UserSettings.setCurrentDirectory(m_currentDirectory.getAbsolutePath());
        UserSettings.setCurrentDirectorySel(m_fileList.getSelectionString());
        UserSettings.setVerticalSplitterPos(m_splitter.getDividerLocation());
        UserSettings.setHorizontalSplitterPos(m_filePane.getDividerLocation());

        UserSettings.save();
        System.exit(0);
    }

    private void buildMenuBar()
    {
        JMenuBar menuBar = new JMenuBar();

        menuBar.add(buildMenu("MainWnd.menu.File", m_fileMenu));
        menuBar.add(buildMenu("MainWnd.menu.Select", m_selectMenu));
        menuBar.add(buildMenu("MainWnd.menu.Operation", m_operationMenu));
        menuBar.add(buildMenu("MainWnd.menu.Help", m_helpMenu));

        setJMenuBar(menuBar);
    }

    private JMenu buildMenu(String strKey, Action[] contents)
    {
        Text.MenuInfo info = Text.getMenuInfo(strKey);

        JMenu menu = new JMenu(info.getName());
        if (info.getMnemonic() != null)
        {
            menu.setMnemonic(info.getMnemonic().intValue());
        }

        for (int i = 0; i < contents.length; i++)
        {
            if (contents[i] == null)
            {
                menu.addSeparator();
            }
            else
            {
                menu.add(new JMenuItem(contents[i]));
            }
        }

        return(menu);
    }

    private class MainWindowListener extends WindowAdapter
    {
        public MainWindowListener()
        {
        }

        public void windowClosing(WindowEvent e)
        {
            MainWnd.this.shutdown();
        }
    }

    private class FileListSelectionListener implements FileListListener
    {
        public FileListSelectionListener()
        {
        }

        public void selectionChanged(FileListEntry item)
        {
            MainWnd.this.setSelectedFile(item);
        }

        public void checksChanged(int nChecked)
        {
            updateFilePaneButtons();
        }
    }

    private class ButtonChangeDirectoryAction extends AbstractAction
    {
        public ButtonChangeDirectoryAction()
        {
            putValue(Action.SMALL_ICON, UIManager.getIcon("Tree.closedIcon"));
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWnd.this.changeDirectory();
        }

        public void setCurrentDirectory(File currentDirectory)
        {
            String strName = currentDirectory.getName();
            if (strName == null || strName.length() == 0)
            {
                strName = currentDirectory.getAbsolutePath();
            }
            putValue(Action.NAME, strName);
        }
    }

    private class FileChangeDirectoryAction extends WispAction
    {
        public FileChangeDirectoryAction()
        {
            super("MainWnd.menu.File.ChangeDir");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWnd.this.changeDirectory();
        }
    }

    private class FileGlobalSettingsAction extends WispAction
    {
        public FileGlobalSettingsAction()
        {
            super("MainWnd.menu.File.GlobalSettings");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (DialogGlobals.run(MainWnd.this, m_executables, m_globalArguments))
            {
                UserSettings.setExecutables(m_executables);
                UserSettings.setArguments(m_globalArguments);
                UserSettings.save();
            }
        }
    }

    private class FileDirectorySettingsAction extends WispAction
    {
        public FileDirectorySettingsAction()
        {
            super("MainWnd.menu.File.DirectorySettings");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (DialogDirectory.run(MainWnd.this, m_directoryArguments, m_globalArguments))
            {
                UserSettings.saveDirectoryArguments(m_currentDirectory,
                                                    m_directoryArgumentFileName,
                                                    m_directoryArguments);
            }
        }
    }

    private class FileExitAction extends WispAction
    {
        public FileExitAction()
        {
            super("MainWnd.menu.File.Exit");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWnd.this.shutdown();
        }
    }

    private class SelectAllAction extends WispAction
    {
        public SelectAllAction()
        {
            super("MainWnd.menu.Select.All");
        }

        public void actionPerformed(ActionEvent event)
        {
            m_fileList.checkAll();
        }
    }

    private class SelectNoneAction extends WispAction
    {
        public SelectNoneAction()
        {
            super("MainWnd.menu.Select.None");
        }

        public void actionPerformed(ActionEvent event)
        {
            m_fileList.checkNone();
        }
    }

    private class SelectInvertAction extends WispAction
    {
        public SelectInvertAction()
        {
            super("MainWnd.menu.Select.Invert");
        }

        public void actionPerformed(ActionEvent event)
        {
            m_fileList.invertChecks();
        }
    }

    private abstract class OperationAction extends WispAction
    {
        public OperationAction(String s)
        {
            super(s);
        }

        protected boolean areSettingsReady(ActionEvent event)
        {
            if (m_executables.getTranslator() == null ||
                m_executables.getTranslator().length() == 0 ||
                m_executables.getCompiler() == null ||
                m_executables.getCompiler().length() == 0)
            {
                m_fileGlobalSettingsAction.actionPerformed(event);
                return(false);
            }

            if (m_globalArguments.getTranslatorArgs() == null ||
                m_globalArguments.getTranslatorArgs().length() == 0 ||
                m_globalArguments.getCompilerArgs() == null ||
                m_globalArguments.getCompilerArgs().length() == 0)
            {
                m_fileGlobalSettingsAction.actionPerformed(event);
                return(false);
            }

            if (m_directoryArguments.getUseGlobal())
            {
                return(true);
            }

            if (m_directoryArguments.getTranslatorArgs() == null ||
                m_directoryArguments.getTranslatorArgs().length() == 0 ||
                m_directoryArguments.getCompilerArgs() == null ||
                m_directoryArguments.getCompilerArgs().length() == 0)
            {
                m_fileDirectorySettingsAction.actionPerformed(event);
                return(false);
            }

            return(true);
        }

        protected void execute(FileListEntry[] entries, int nOperationType)
        {
            if (entries == null || entries.length == 0)
            {
                return;
            }

            Operation op = new Operation(   MainWnd.this,
                                            nOperationType,
                                            entries,
                                            m_executables,
                                            m_globalArguments,
                                            m_directoryArguments);
            op.start(); // blocks

            m_fileList.updateAll();
            MainWnd.this.setSelectedFile(m_fileList.getSelectedEntry());
        }
    }

    private class OperationTranslateAction extends OperationAction
    {
        public OperationTranslateAction()
        {
            super("MainWnd.menu.Operation.Translate");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (!areSettingsReady(event))
            {
                return;
            }

            FileListEntry[] entries = m_fileList.getCheckedEntries();
            execute(entries, Operation.TRANSLATE);
        }
    }

    private class OperationCompileAction extends OperationAction
    {
        public OperationCompileAction()
        {
            super("MainWnd.menu.Operation.Compile");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (!areSettingsReady(event))
            {
                return;
            }
            FileListEntry[] entries = m_fileList.getCheckedEntries();
            int nCount = entries.length;
            ArrayList missingCobolFiles = new ArrayList();
            for (int i = 0; i < nCount; i++)
            {
                File cobolFile = entries[i].getCobolFile();
                if (!cobolFile.exists())
                {
                    missingCobolFiles.add(entries[i].getCobolFileName());
                }
            }

            if (missingCobolFiles.size() >= nCount)
            {
                JOptionPane.showMessageDialog(  MainWnd.this,
                                                Text.getString("DialogMissingCobolFiles.all"),
                                                Text.getString("DialogMissingCobolFiles.title"),
                                                JOptionPane.ERROR_MESSAGE);
                return;
            }

            if (!DialogMissingCobolFiles.run(MainWnd.this, missingCobolFiles))
            {
                return;
            }

            execute(entries, Operation.COMPILE);
        }
    }

    private class OperationTranslateCompileAction extends OperationAction
    {
        public OperationTranslateCompileAction()
        {
            super("MainWnd.menu.Operation.TranslateCompile");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (!areSettingsReady(event))
            {
                return;
            }

            FileListEntry[] entries = m_fileList.getCheckedEntries();
            execute(entries, Operation.TRANSLATE_AND_COMPILE);
        }
    }

    private class HelpAboutAction extends WispAction
    {
        public HelpAboutAction()
        {
            super("MainWnd.menu.Help.About");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogAbout.run(MainWnd.this);
        }
    }

    private FileChangeDirectoryAction m_fileChangeDirectoryAction =  new FileChangeDirectoryAction();
    private ButtonChangeDirectoryAction m_buttonChangeDirectoryAction = new ButtonChangeDirectoryAction();
    private FileGlobalSettingsAction    m_fileGlobalSettingsAction = new FileGlobalSettingsAction();
    private FileDirectorySettingsAction m_fileDirectorySettingsAction = new FileDirectorySettingsAction();

    private Action[] m_fileMenu =
    {
        m_fileChangeDirectoryAction,
        null,
        m_fileGlobalSettingsAction,
        m_fileDirectorySettingsAction,
        null,
        new FileExitAction()
    };

    private Action[] m_selectMenu =
    {
        new SelectAllAction(),
        new SelectNoneAction(),
        new SelectInvertAction()
    };

    private Action[] m_operationMenu =
    {
        new OperationTranslateAction(),
        new OperationCompileAction(),
        new OperationTranslateCompileAction()
    };

    private Action[] m_helpMenu =
    {
        new HelpAboutAction()
    };

    private File            m_currentDirectory;
    private FileList        m_fileList;
    private FilePane        m_filePane;
    private JSplitPane      m_splitter;
    private JButton         m_dirChangeButton;
    private Executables     m_executables;
    private Arguments       m_globalArguments;
    private Arguments       m_directoryArguments;

    private static final String m_directoryArgumentFileName = "WISPer.settings";
}