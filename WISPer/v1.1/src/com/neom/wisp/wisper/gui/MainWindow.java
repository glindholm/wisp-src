package com.neom.wisp.wisper.gui;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.text.MessageFormat;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

import com.neom.util.gui.DirectoryChooserDialog;
import com.neom.wisp.wisper.DirectoryOptions;
import com.neom.wisp.wisper.ProgramSpec;
import com.neom.wisp.wisper.SourceModule;
import com.neom.wisp.wisper.SourceModuleList;
import com.neom.wisp.wisper.UserSettings;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */

public class MainWindow extends JFrame
{
	public MainWindow()
	{
		m_sourceModuleList = new SourceModuleList();
		
		setName("MainWindow");
        setIconImage(Images.getImage(Images.WISPerIconSmall));
        
        ensureSettings();
        
		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener(new CloseListener());
		
        buildMenuBar();
        
        m_splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, 
        							true, 
        							buildFilePanel(), 
        							buildErrorPanel());
        							
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
        
        Main.setMinSize(this);
        String strCurrentDirectory = UserSettings.getCurrentDirectory();
        if (strCurrentDirectory == null)
        {
            strCurrentDirectory = System.getProperty("user.home");
        }
        
        File fileCurrentDirectory = new File(strCurrentDirectory);
        if (!fileCurrentDirectory.exists() || !fileCurrentDirectory.isDirectory())
        {
        	fileCurrentDirectory = new File(System.getProperty("user.home"));
        }
        
        setDirectory(fileCurrentDirectory);
        m_filePane.restoreChecks(UserSettings.getCheckedFiles());
	}
	
	public SourceModuleList getSourceModuleList()
	{
		return(m_sourceModuleList);
	}

    private void ensureSettings()
    {
    	ProgramSpec specTranslator = UserSettings.getTranslatorSpec();
    	ProgramSpec specCompiler = UserSettings.getCompilerSpec();
    	ProgramSpec specExecuter = UserSettings.getExecuterSpec();
    	
        if (!specTranslator.isValid() ||
        	!specCompiler.isValid())
        {
	        if (!DialogGlobals.run(this, specTranslator, specCompiler, specExecuter))
	        {
	        	System.exit(0);
	        }
	        
	        UserSettings.setTranslatorSpec(specTranslator);
	        UserSettings.setCompilerSpec(specCompiler);
	        UserSettings.setExecuterSpec(specExecuter);
	        UserSettings.save();
        }
    }

    private void setDirectory(File dir)
    {
        Cursor oldCursor = getCursor();
        setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );

        m_sourceModuleList.setCurrentDirectory(dir);
        m_buttonChangeDirectoryAction.setCurrentDirectory(dir);
        m_filePane.load(m_sourceModuleList);

        updateTitle();
        
        setCursor(oldCursor);
    }
    
    private void changeDirectory()
    {
    	File currentDirectory = m_sourceModuleList.getCurrentDirectory();
    	
        DirectoryChooserDialog dlg = new DirectoryChooserDialog(this);
        dlg.setDirectory(currentDirectory);
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
        if (currentDirectory != null && currentDirectory.equals(theNewDirectory))
        {
            return;
        }
        
        setDirectory(dlg.getDirectory());
    }

    private void updateTitle()
    {
        String template = Text.getString("MainWindow.title");
        String[] args = new String[1];
        args[0] = m_sourceModuleList.getCurrentDirectory().getAbsolutePath();
        String title = MessageFormat.format(template, args);
        setTitle(title);
    }
    
    private JComponent buildFilePanel()
    {
    	m_filePane = new MainWindowFilePane(this);
    	
    	JButton changeDirectoryButton = new JButton(m_buttonChangeDirectoryAction);
    	
        Box buttonPanel = Box.createHorizontalBox();
        for (int i = 0; i < m_selectMenu.length; i++)
        {
            buttonPanel.add(new JButton(m_selectMenu[i]));
        }
        
    	JPanel filePanel = new JPanel(new BorderLayout());
    	filePanel.add(changeDirectoryButton, BorderLayout.NORTH);
    	filePanel.add(new JScrollPane(m_filePane), BorderLayout.CENTER);
        filePanel.add(buttonPanel, BorderLayout.SOUTH);
        
    	return(filePanel);
    }

    private JComponent buildErrorPanel()
    {
    	m_errorPane = new ErrorPane();
    	
        Box buttonPanel = Box.createHorizontalBox();
        for (int i = 0; i < m_selectMenu.length; i++)
        {
            buttonPanel.add(new JButton(m_operationMenu[i]));
        }
        
    	JScrollPane scrollPane = new JScrollPane(m_errorPane);
    	
    	JPanel errorPanel = new JPanel(new BorderLayout());
    	errorPanel.add(buttonPanel, BorderLayout.NORTH);
    	errorPanel.add(scrollPane, BorderLayout.CENTER);
    	
    	return(errorPanel);
    }

    private void buildMenuBar()
    {
        JMenuBar menuBar = new JMenuBar();

        menuBar.add(Main.buildMenu("MainWindow.menu.File", m_fileMenu));
        menuBar.add(Main.buildMenu("MainWindow.menu.Select", m_selectMenu));
        menuBar.add(Main.buildMenu("MainWindow.menu.Operation", m_operationMenu));
        Text.MenuInfo info = Text.getMenuInfo("MainWindow.menu.Options");

        JMenu optionsMenu = new JMenu(info.getName());
        if (info.getMnemonic() != null)
        {
            optionsMenu.setMnemonic(info.getMnemonic().intValue());
        }
        
        m_menuItemAllowCobEdit = new JCheckBoxMenuItem(m_optionsAllowCobEdit);
        optionsMenu.add(m_menuItemAllowCobEdit);
		menuBar.add(optionsMenu);

        menuBar.add(Main.buildMenu("MainWindow.menu.Help", m_helpMenu));
        setJMenuBar(menuBar);
    }

	public void checksChanged(int nChecked)
	{
		boolean bEnabled = true;
		
		if (nChecked == 0)
		{
			bEnabled = false;
		}
		
        for (int i = 0; i < m_operationMenu.length; i++)
        {
        	if (m_operationMenu[i] != null)
        	{
            	m_operationMenu[i].setEnabled(bEnabled);
        	}
        }
	}
	
    private void doExecute(int nOperation)
    {
    	DialogProgress dlg = new DialogProgress(this);
    	SourceModule[] list = m_filePane.getCheckedModules();
    	if (list.length < 1)
    	{
    		return;
    	}
    	
    	CompileTranslateExecuter exe = new CompileTranslateExecuter(list, dlg, nOperation);
    	exe.execute();	// blocks
    	dlg.dispose();
    	for (int i = 0; i < list.length; i++)
    	{
    		list[i].saveProperties();
    	}
    	
    	m_errorPane.setContents(list);
    }
    
    private void onFileGlobalSettings()
    {
    	ProgramSpec specTranslator = UserSettings.getTranslatorSpec();
    	ProgramSpec specCompiler = UserSettings.getCompilerSpec();
    	ProgramSpec specExecuter = UserSettings.getExecuterSpec();
    	
        if (DialogGlobals.run(this, specTranslator, specCompiler, specExecuter))
        {
	        UserSettings.setTranslatorSpec(specTranslator);
	        UserSettings.setCompilerSpec(specCompiler);
	        UserSettings.setExecuterSpec(specExecuter);
            UserSettings.save();
            m_sourceModuleList.fireSettingsChanged();
        }
    }
    
    private void onFileDirectorySettings()
    {
    	File currentDirectory = m_sourceModuleList.getCurrentDirectory();
    	DirectoryOptions options = UserSettings.getDirectoryOptions(currentDirectory);
    	ProgramSpec specTranslator = UserSettings.getTranslatorSpec();
    	ProgramSpec specCompiler = UserSettings.getCompilerSpec();
    	
        if (DialogDirectory.run(this, options, specTranslator, specCompiler))
        {
        	UserSettings.setDirectoryOptions(currentDirectory, options);
        	UserSettings.save();
        }
    }
    
	private void onOptionsAllowCobEdit()
	{
    	if (!UserSettings.getAllowCobEdit())
    	{
    		int nResult = JOptionPane.showConfirmDialog(this,
    									Text.getString("MainWindow.msg.AllowCobEdit"),
    									Text.getString("MainWindow.title.AllowCobEdit"),
    									JOptionPane.YES_NO_OPTION,
    									JOptionPane.QUESTION_MESSAGE);
    		if (nResult != 0)
    		{
    			return;
    		}
    	}
    	
    	UserSettings.setAllowCobEdit(!UserSettings.getAllowCobEdit());
    	m_menuItemAllowCobEdit.setSelected(UserSettings.getAllowCobEdit());
    	m_sourceModuleList.fireSettingsChanged();
	}
	
	private void onOperationClearResults()
	{
		int nResult = JOptionPane.showConfirmDialog(this,
									Text.getString("MainWindow.msg.ClearResults"),
									Text.getString("MainWindow.title.ClearResults"),
									JOptionPane.YES_NO_OPTION,
									JOptionPane.QUESTION_MESSAGE);
		if (nResult != 0)
		{
			return;
		}
		
    	SourceModule[] modules = m_sourceModuleList.getModules();
    	DialogProgress dlg = new DialogProgress(this);
    	ClearResultsExecuter exe = new ClearResultsExecuter(modules, dlg);
    	exe.execute();	// blocks
    	dlg.dispose();

    	m_errorPane.clearContents();
	}
	
    private void onFileExit()
    {
    	SourceModule[] modules = m_sourceModuleList.getModules();
    	for (int i = 0; i < modules.length; i++)
    	{
    		SourceWindow w = modules[i].getSourceWindow();
    		if (w != null)
    		{
    			if (!w.closeWindow())
    			{
    				return;
    			}
    		}
    	}
    	
        Dimension size = getSize();
        UserSettings.setMainWndWidth(size.width);
        UserSettings.setMainWndHeight(size.height);

        Point location = getLocation();
        UserSettings.setMainWndX(location.x);
        UserSettings.setMainWndY(location.y);

        UserSettings.setCurrentDirectory(m_sourceModuleList.getCurrentDirectory().getAbsolutePath());
        UserSettings.setCheckedFiles(m_filePane.getCheckedFileNames());
        UserSettings.setVerticalSplitterPos(m_splitter.getDividerLocation());

        UserSettings.save();
        System.exit(0);
    }

	private SourceModuleList		m_sourceModuleList;
	private JSplitPane				m_splitter;
	private MainWindowFilePane		m_filePane;
	private ErrorPane				m_errorPane;
	private boolean				m_bDeferUpdates = false;
	
	private class CloseListener extends WindowAdapter
	{
		public void windowClosing(WindowEvent e)
		{
			MainWindow.this.onFileExit();
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
            MainWindow.this.changeDirectory();
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

    private class FileChangeDirectoryAction extends AbstractAction
    {
        public FileChangeDirectoryAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.File.ChangeDir");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWindow.this.changeDirectory();
        }
    }

    private class FileGlobalSettingsAction extends AbstractAction
    {
        public FileGlobalSettingsAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.File.GlobalSettings");
        }

        public void actionPerformed(ActionEvent event)
        {
        	MainWindow.this.onFileGlobalSettings();
        }
    }

    private class FileDirectorySettingsAction extends AbstractAction
    {
        public FileDirectorySettingsAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.File.DirectorySettings");
        }

        public void actionPerformed(ActionEvent event)
        {
        	MainWindow.this.onFileDirectorySettings();
        }
    }

    private class FileExitAction extends AbstractAction
    {
        public FileExitAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.File.Exit");
        }

        public void actionPerformed(ActionEvent event)
        {
			MainWindow.this.onFileExit();
        }
    }

    private class SelectAllAction extends AbstractAction
    {
        public SelectAllAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Select.All");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWindow.this.m_filePane.checkAll();
        }
    }

    private class SelectNoneAction extends AbstractAction
    {
        public SelectNoneAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Select.None");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWindow.this.m_filePane.checkNone();
        }
    }

    private class SelectInvertAction extends AbstractAction
    {
        public SelectInvertAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Select.Invert");
        }

        public void actionPerformed(ActionEvent event)
        {
            MainWindow.this.m_filePane.invertChecks();
        }
    }

	private class OperationTranslateAction extends AbstractAction
    {
        public OperationTranslateAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Operation.Translate");
    		Images.setActionIcon(this, Images.ButtonTranslate);
        }

        public void actionPerformed(ActionEvent event)
        {
			MainWindow.this.doExecute(CompileTranslateExecuter.OPERATION_TRANSLATE);
        }
    }

    private class OperationCompileAction extends AbstractAction
    {
        public OperationCompileAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Operation.Compile");
    		Images.setActionIcon(this, Images.ButtonCompile);
        }

        public void actionPerformed(ActionEvent event)
        {
			MainWindow.this.doExecute(CompileTranslateExecuter.OPERATION_COMPILE);
        }
    }

    private class OperationTranslateCompileAction extends AbstractAction
    {
        public OperationTranslateCompileAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Operation.TranslateCompile");
    		Images.setActionIcon(this, Images.ButtonTranComp);
        }

        public void actionPerformed(ActionEvent event)
        {
			MainWindow.this.doExecute(CompileTranslateExecuter.OPERATION_TRANSLATE_AND_COMPILE);
        }
    }

    private class OperationClearResultsAction extends AbstractAction
    {
        public OperationClearResultsAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Operation.ClearResults");
        }

        public void actionPerformed(ActionEvent event)
        {
			MainWindow.this.onOperationClearResults();
        }
    }

    private class OptionsAllowCobEdit extends AbstractAction
    {
        public OptionsAllowCobEdit()
        {
            Text.setActionMenu(this, "MainWindow.menu.Options.AllowCobEdit");
        }

        public void actionPerformed(ActionEvent event)
        {
        	MainWindow.this.onOptionsAllowCobEdit();
        }
    }

    private class HelpAboutAction extends AbstractAction
    {
        public HelpAboutAction()
        {
            Text.setActionMenu(this, "MainWindow.menu.Help.About");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogAbout.run(MainWindow.this);
        }
    }

    private ButtonChangeDirectoryAction m_buttonChangeDirectoryAction = new ButtonChangeDirectoryAction();
	private OptionsAllowCobEdit		 m_optionsAllowCobEdit = new OptionsAllowCobEdit();
	private JMenuItem					 m_menuItemAllowCobEdit;
	
    private Action[] m_fileMenu =
    {
        new FileChangeDirectoryAction(),
        null,
        new FileGlobalSettingsAction(),
        new FileDirectorySettingsAction(),
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
        new OperationTranslateCompileAction(),
        null,
        new OperationClearResultsAction()
    };

    private Action[] m_helpMenu =
    {
        new HelpAboutAction()
    };
}
