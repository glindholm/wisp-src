package com.neom.wisp.wisper.gui;

import com.neom.wisp.wisper.*;
import com.neom.util.gui.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.text.*;
import javax.swing.text.*;
import java.io.*;
import javax.swing.border.*;
import java.awt.datatransfer.*;

/**
 * @author khunter
 *
 */

public class SourceWindow extends JFrame implements SourceModuleListener
{
	public SourceWindow(SourceModule module)
	{
		m_sourceModule = module;
		m_sourceModule.setSourceWindow(this);
		m_sourceModule.addSourceModuleListener(this);
		
		setName("SourceWindow");
        setIconImage(Images.getImage(Images.WISPerIconSmall));
        
        String template = Text.getString("SourceWindow.title");
        String[] args = new String[1];
        args[0] = module.getSourceFile().getName();
        String title = MessageFormat.format(template, args);
        setTitle(title);
        
		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener(new CloseListener());
		
		buildMenuBar();
		
        m_tabPane = new JTabbedPane();
        m_tabPane.setOpaque(true);
        m_tabPane.setFont(Main.getTabTitleFont());
		
		m_docSource = new PlainDocument();
		m_docSource.addDocumentListener(new SourceDocumentListener());
		
		m_docTranslated = new PlainDocument();
		m_docTranslated.addDocumentListener(new TranslatedDocumentListener());
		
        if (!loadFile(m_docSource, m_sourceModule.getSourceFile(), null))
        {
            showCantReadError(m_sourceModule.getSourceFile().getName());
            dispose();
            return;
        }
        setSourceDirty(false);
        
        m_taSource = createTextArea(m_docSource);
        m_taSource.setEditable(true);
        m_taTranslated = createTextArea(m_docTranslated);
        m_taTranslated.setEditable(UserSettings.getAllowCobEdit());

		showSourceTab();

        if (loadFile(m_docTranslated, m_sourceModule.getTranslatedFile(), m_taTranslated))
        {
        	showTranslatedTab();
        	setTranslatedDirty(false);
        }

		m_errorPane = new ErrorPane();
		m_errorPane.setContents(m_sourceModule);
		JScrollPane errorScroll = new JScrollPane(m_errorPane);
		m_splitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true);
		m_splitter.setResizeWeight(1.0);
		
        m_splitter.setTopComponent(m_tabPane);
        m_splitter.setBottomComponent(errorScroll);
        
		Container c = getContentPane();
		c.setLayout(new BorderLayout());
		c.add(buildToolbar(), BorderLayout.NORTH);
		c.add(m_splitter, BorderLayout.CENTER);
		
		settingsChanged();
		
		pack();
		
        Main.setMinSize(this);
        Main.setMaxSize(this);
        Main.centerInScreen(this);
        
        validate();
        
        m_splitter.setDividerLocation(0.8);
        
		setVisible(true);
		
		m_tabPane.addChangeListener(new TabSelectionListener());
		m_taSource.requestFocus();
	}
	
	public boolean closeWindow()
	{
		if (isSourceDirty())
		{
			String[] fileNameInsert = { m_sourceModule.getSourceFile().getName() };
			int nResult = JOptionPane.showConfirmDialog(this,
														Text.getMessage("SourceWindow.msg.SaveChanges", fileNameInsert),
														Text.getString("SourceWindow.title.SaveChanges"),
														JOptionPane.YES_NO_CANCEL_OPTION,
														JOptionPane.WARNING_MESSAGE);
			switch(nResult)
			{
			case 0:	// yes
				if (!saveSource())
				{
					return(false);
				}
				break;
			case 1:	// no
				break;
			case 2:	// cancel
			default:
				return(false);
			}
		}
		
		if (isTranslatedDirty())
		{
			String[] fileNameInsert = { m_sourceModule.getTranslatedFile().getName() };
			int nResult = JOptionPane.showConfirmDialog(this,
														Text.getMessage("SourceWindow.msg.SaveChanges", fileNameInsert),
														Text.getString("SourceWindow.title.SaveChanges"),
														JOptionPane.YES_NO_CANCEL_OPTION,
														JOptionPane.WARNING_MESSAGE);
			switch(nResult)
			{
			case 0:	// yes
				if (!saveTranslated())
				{
					return(false);
				}
				break;
			case 1:	// no
				break;
			case 2:	// cance;
			default:
				return(false);
			}
		}
		
		m_sourceModule.setSourceWindow(null);
		m_sourceModule.removeSourceModuleListener(this);
		setVisible(false);
		dispose();
		return(true);
	}
	
	public void settingsChanged()
	{
		ProgramSpec specExecuter = UserSettings.getExecuterSpec();
		String strTemplate = specExecuter.getArgumentTemplate();
		if (strTemplate != null && 
			strTemplate.length() > 0 && 
			specExecuter.isTemplateValid())
		{
			m_buttonExecute.setEnabled(true);
			m_actionExecute.setEnabled(true);
		}
		else
		{
			m_buttonExecute.setEnabled(false);
			m_actionExecute.setEnabled(false);
		}
		
        m_taTranslated.setEditable(UserSettings.getAllowCobEdit());
	}
	
	public boolean isSourceDirty()
	{
		return(m_bSourceDirty);
	}
	
	public void setSourceDirty(boolean bDirty)
	{
		if (m_bSourceDirty == bDirty)
		{
			return;	// no change
		}
		
		if (m_tabPane.getTabCount() >= 1)
		{
			if (bDirty)
			{
				m_tabPane.setTitleAt(0, m_sourceModule.getSourceFile().getName() + " *");
			}
			else
			{
				m_tabPane.setTitleAt(0, m_sourceModule.getSourceFile().getName() + "  ");
			}
		}
		
		m_bSourceDirty = bDirty;
	}
	
	public boolean isTranslatedDirty()
	{
		return(m_bTranslatedDirty);
	}
	
	public void setTranslatedDirty(boolean bDirty)
	{
		if (m_bTranslatedDirty == bDirty)
		{
			return;	// no change
		}
		
		if (m_tabPane.getTabCount() >= 2)
		{
			if (bDirty)
			{
				m_tabPane.setTitleAt(1, m_sourceModule.getTranslatedFile().getName() + " *");
			}
			else
			{
				m_tabPane.setTitleAt(1, m_sourceModule.getTranslatedFile().getName() + "  ");
			}
		}
		
		m_bTranslatedDirty = bDirty;
	}
	
    private void buildMenuBar()
    {
        JMenuBar menuBar = new JMenuBar();

        menuBar.add(Main.buildMenu("SourceWindow.menu.File", m_fileMenu));
        menuBar.add(Main.buildMenu("SourceWindow.menu.Edit", m_editMenu));
        menuBar.add(Main.buildMenu("SourceWindow.menu.Operation", m_operationMenu));
        menuBar.add(Main.buildMenu("SourceWindow.menu.Help", m_helpMenu));

        setJMenuBar(menuBar);
    }
    
    private JToolBar buildToolbar()
    {
    	JToolBar toolBar = new JToolBar();
    	
    	toolBar.setFloatable(false);
    	
    	toolBar.add(new JButton(new ButtonSaveAction()));
    	
    	toolBar.addSeparator();
    	
    	toolBar.add(new JButton(new ButtonCutAction()));
    	toolBar.add(new JButton(new ButtonCopyAction()));
    	toolBar.add(new JButton(new ButtonPasteAction()));
    	
    	toolBar.addSeparator();
    	
    	toolBar.add(new JButton(new ButtonGotoAction()));
    	
    	toolBar.addSeparator();
    	
    	toolBar.add(new JButton(new ButtonFindAction()));
    	toolBar.add(new JButton(m_buttonFindAgain));
    	
    	toolBar.addSeparator();
    	
    	toolBar.add(new JButton(new ButtonTranslateAction()));
    	toolBar.add(new JButton(new ButtonCompileAction()));
    	toolBar.add(new JButton(new ButtonTranslateCompileAction()));
    	
    	toolBar.addSeparator();
    	
    	toolBar.add(new JButton(m_buttonExecute));
    	return(toolBar);
    }
    
    private JTextArea createTextArea(Document doc)
    {
        JTextArea textArea = new JTextArea(doc);
        textArea.setColumns(80);
        textArea.setEditable(true);
        ActionMap map = textArea.getActionMap();
        map.put(m_insertTabAction.getValue(Action.NAME), m_insertTabAction);
        return(textArea);
    }

	public void moduleTranslated()
	{
	    if (!loadFile(m_docTranslated, m_sourceModule.getTranslatedFile(), m_taTranslated))
	    {
        	showCantReadError(m_sourceModule.getTranslatedFile().getName());
        	hideTranslatedTab();
	    }
	    else
	    {
	    	showTranslatedTab();
	    	m_errorPane.setContents(m_sourceModule);
        	setTranslatedDirty(false);
	    }
	}
	
	public void moduleCompiled()
	{
		m_errorPane.setContents(m_sourceModule);
	}
	
	public void moduleCleared()
	{
		m_errorPane.setContents(m_sourceModule);
	}
	
    private void showCantReadError(String fileName)
    {
        String[] input = { fileName };
        JOptionPane.showMessageDialog(	null,
                                        Text.getMessage("SourceWindow.error.CantReadFile", input),
                                        Text.getString("SourceWindow.title.Error"),
                                        JOptionPane.ERROR_MESSAGE);
    }
    
    private void addTab(String strName, JTextArea ta)
    {
    	SourcePanel sp = new SourcePanel(ta);
    	JScrollPane scroller = new JScrollPane(sp);
    	
    	JScrollBar vertBar = scroller.getVerticalScrollBar();
    	vertBar.setUnitIncrement(sp.getCharHeight());
    	
    	JScrollBar horzBar = scroller.getHorizontalScrollBar();
    	horzBar.setUnitIncrement(sp.getCharWidth() * 4);
    	
    	m_tabPane.addTab(strName + "  ", scroller);
    }
    
    private void showSourceTab()
    {
    	if (m_tabPane.getTabCount() > 0)
    	{
    		return;
    	}
    	
        addTab(m_sourceModule.getSourceFile().getName(), m_taSource);
    }
    
    private void showTranslatedTab()
    {
    	if (m_tabPane.getTabCount() > 1)
    	{
    		return;
    	}
    	
        addTab(m_sourceModule.getTranslatedFile().getName(), m_taTranslated);
    }
    
    private void hideTranslatedTab()
    {
    	if (m_tabPane.getTabCount() < 2)
    	{
    		return;
    	}
    	
    	m_tabPane.remove(1);
    }

    private boolean loadFile(Document doc, File srcFile, JTextComponent textComp)
    {
    	int nCaretPos = 0;
    	if (textComp != null)
    	{
    		nCaretPos = textComp.getCaretPosition();
    	}
    	
		try
		{
			if (doc.getLength() > 0)
			{
				doc.remove(0, doc.getLength());
			}
		}
		catch(BadLocationException e)
		{
		}
		
    	if (m_inputBuffer == null)
    	{
    		m_inputBuffer = new char[4096];
    	}
    	
        FileInputStream is = null;
        InputStreamReader sr = null;
		LineNumberReader lr = null;
		
        try
        {
            is = new FileInputStream(srcFile);
            sr = new InputStreamReader(is);
            lr = new LineNumberReader(sr);
            
            for(;;)
            {
                String strLine = lr.readLine();
                if (strLine == null)
                {
                	break;
                }
                
				doc.insertString(doc.getLength(), strLine, null);
				doc.insertString(doc.getLength(), "\n", null);
            }
            
            if (nCaretPos > doc.getLength())
            {
            	nCaretPos = doc.getLength();
            }
            
            if (textComp != null)
            {
            	textComp.setCaretPosition(nCaretPos);
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
                if (lr != null)
                {
                    lr.close();
                }
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
    
    private boolean saveFile(JTextArea ta, Document doc, File fileDest)
    {
    	FileOutputStream os = null;
    	PrintWriter pw = null;
    	BufferedOutputStream bos = null;
    	
    	try
    	{
    		os = new FileOutputStream(fileDest);
    		bos = new BufferedOutputStream(os);
    		pw = new PrintWriter(bos);
    		
    		int nLines = ta.getLineCount();
    		for (int i = 0; i < nLines; i++)
    		{
    			int nStart = ta.getLineStartOffset(i);
    			int nEnd = ta.getLineEndOffset(i);
    			String strLine = doc.getText(nStart, nEnd - nStart -1); // omit '\n' at end
    			pw.println(strLine);
    		}
    	}
    	catch(IOException e)
    	{
    	}
    	catch(BadLocationException e)
    	{
    	}
        finally
        {
            try
            {
                if (pw != null)
                {
                    pw.close();
                }
                if (bos != null)
                {
                    bos.close();
                }
                if (os != null)
                {
                    os.close();
                }
            }
            catch(Exception e)
            {
            }
        }

        return(true);
    }
    
    private void doExecute(int nOperation)
    {
    	DialogProgress dlg = new DialogProgress(this);
    	SourceModule[] list = { m_sourceModule };
    	CompileTranslateExecuter exe = new CompileTranslateExecuter(list, dlg, nOperation);
    	exe.execute();	// blocks
    	dlg.dispose();
    	if (!m_sourceModule.saveProperties())
    	{
    		String[] subst = { m_sourceModule.getPropertiesFile().getAbsolutePath() };
    		
    		JOptionPane.showMessageDialog(	this,
    										Text.getMessage("SourceWindow.msg.ProcessSaveFailure", subst),
    										Text.getString("SourceWindow.title.Error"),
    										JOptionPane.ERROR_MESSAGE);
    	}
    }
    
	private boolean saveSource()
	{
		boolean bSuccess = saveFile(m_taSource, m_docSource, m_sourceModule.getSourceFile());
		if (bSuccess)
		{
			setSourceDirty(false);
		}
		else
		{
			String[] fileNameInsert = { m_sourceModule.getSourceFile().getName() };
			JOptionPane.showMessageDialog(	this, 
											Text.getMessage("SourceWindow.error.CantWriteFile", fileNameInsert), 
											Text.getString("SourceWindow.title.Error"), 
											JOptionPane.ERROR_MESSAGE);
		}
		m_taSource.requestFocus();
		return(bSuccess);
	}
	
	private boolean saveTranslated()
	{
        boolean bSuccess = saveFile(m_taTranslated, m_docTranslated, m_sourceModule.getTranslatedFile());
		if (bSuccess)
		{
			setTranslatedDirty(false);
		}
		else
		{
			String[] fileNameInsert = { m_sourceModule.getTranslatedFile().getName() };
			JOptionPane.showMessageDialog(	this, 
											Text.getMessage("SourceWindow.error.CantWriteFile", fileNameInsert), 
											Text.getString("SourceWindow.title.Error"), 
											JOptionPane.ERROR_MESSAGE);
		}
		m_taTranslated.requestFocus();
		return(bSuccess);
	}
	
	private void doFind(JTextArea textArea, String strText, boolean bIgnoreCase)
	{
		int nSelectionStart = textArea.getSelectionStart();
		int nSelectionEnd = textArea.getSelectionEnd();
		int nStartPosition;
		
		if (nSelectionStart != nSelectionEnd)
		{
			nStartPosition = Math.min(nSelectionStart, nSelectionEnd) + 1;
		}
		else
		{
			nStartPosition = textArea.getCaretPosition();
		}
		
		Document doc = textArea.getDocument();
		int nLength = doc.getLength();
		
		try
		{
			String strDocument = doc.getText(0, nLength);
			if (bIgnoreCase)
			{
				strDocument = strDocument.toLowerCase();
				strText = strText.toLowerCase();
			}
			
			int nIndex = strDocument.indexOf(strText, nStartPosition);
			if (nIndex < 0)
			{
				nStartPosition = 0;
				nIndex = strDocument.indexOf(strText, nStartPosition);
			}
			if (nIndex < 0)
			{
				JOptionPane.showMessageDialog(	this,
												Text.getString("DialogFind.msg.notFound"),
												Text.getString("DialogFind.title.notFound"),
												JOptionPane.ERROR_MESSAGE);
				return;
			}
			
			nSelectionStart = nIndex;
			nSelectionEnd = nSelectionStart + strText.length();
			
			Rectangle r = textArea.modelToView(nSelectionStart);
			
			textArea.repaint();
			textArea.select(nSelectionStart, nSelectionEnd);
			textArea.requestFocus();
		}
		catch(BadLocationException e)
		{
		}
	}
	
	private JTextArea getActiveTextArea()
	{
    	if (m_tabPane.getSelectedIndex() == 0)
    	{
    		return(m_taSource);
    	}
    	
    	return(m_taTranslated);
	}
	
	private void onFileSave()
	{
    	if (m_tabPane.getSelectedIndex() == 0)
    	{
    		saveSource();
    	}
    	else
    	{
    		saveTranslated();
    	}
    	
    	getActiveTextArea().requestFocus();
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
            Main.getMainWindow().getSourceModuleList().fireSettingsChanged();
        }
    	
    	getActiveTextArea().requestFocus();
    }
    
    private void onFileDirectorySettings()
    {
    	File currentDirectory = m_sourceModule.getSourceDirectory();
    	DirectoryOptions options = UserSettings.getDirectoryOptions(currentDirectory);
    	ProgramSpec specTranslator = UserSettings.getTranslatorSpec();
    	ProgramSpec specCompiler = UserSettings.getCompilerSpec();
    	
        if (DialogDirectory.run(this, options, specTranslator, specCompiler))
        {
        	UserSettings.setDirectoryOptions(currentDirectory, options);
        	UserSettings.save();
        }
    	
    	getActiveTextArea().requestFocus();
    }
    
	private void onEditCut()
	{
		JTextArea ta = getActiveTextArea();
		
		ta.cut();
		ta.requestFocus();
	}
	
	private void onEditCopy()
	{
		JTextArea ta = getActiveTextArea();
		
		ta.copy();
		ta.requestFocus();
	}
	
	private void onEditPaste()
	{
		JTextArea ta = getActiveTextArea();
		
		ta.paste();
		ta.requestFocus();
	}
	
	private void onEditFind()
	{
    	if (m_dlgFind == null)
    	{
    		m_dlgFind = new DialogFind(this);
    	}
    	
    	String strSelection = getActiveTextArea().getSelectedText();
    	if (strSelection != null)
    	{
    		m_dlgFind.setFindText(strSelection);
    	}
    	
    	if (!m_dlgFind.run())
    	{
    		return;
    	}
    	
    	String strText = m_dlgFind.getFindText();
    	if (strText == null || strText.length() == 0)
    	{
    		m_actionFindAgain.setEnabled(false);
    		m_buttonFindAgain.setEnabled(false);
    		return;
    	}
    	
    	m_actionFindAgain.setEnabled(true);
    	m_buttonFindAgain.setEnabled(true);
    	
    	doFind(	getActiveTextArea(), 
    			strText, 
    			m_dlgFind.getIgnoreCase());
	}
	
	private void onEditFindAgain()
	{
    	if (m_dlgFind == null)
    	{
    		return;
    	}
    	
    	String strText = m_dlgFind.getFindText();
    	if (strText == null || strText.length() == 0)
    	{
    		return;
    	}
    	
    	doFind(	getActiveTextArea(), 
    			strText, 
    			m_dlgFind.getIgnoreCase());
	}
	
	private void onEditGoto()
	{
    	String strLine = JOptionPane.showInputDialog(this,
										        	Text.getString("SourceWindow.msg.Goto"),
										        	Text.getString("SourceWindow.title.Goto"),
										        	JOptionPane.PLAIN_MESSAGE);
		if (strLine == null)
		{
			return;
		}
		
		int nLineNum = 0;
		JTextArea ta = getActiveTextArea();
		
		try
		{
			nLineNum = Integer.parseInt(strLine.trim()) - 1;
		
        	if (nLineNum < 0)
        	{
        		nLineNum = 0;
        	}
        	
        	if (nLineNum >= ta.getLineCount())
        	{
        		nLineNum = ta.getLineCount() - 1;
        	}
        	
        	ta.setCaretPosition(ta.getLineStartOffset(nLineNum));
        	ta.requestFocus();
        	ta.repaint(0, 0, 0, getWidth(), getHeight());
    	}
		catch(BadLocationException e)
		{
			return;
		}
		catch(NumberFormatException e)
		{
			return;
		}
	}
	
	private void onOperationTranslate()
	{
    	if (isSourceDirty())
    	{
    		if (!saveSource())
    		{
    			return;
    		}
    	}
    	
    	doExecute(CompileTranslateExecuter.OPERATION_TRANSLATE);
    	getActiveTextArea().requestFocus();
	}
	
	private void onOperationCompile()
	{
    	if (isTranslatedDirty())
    	{
    		if (!saveTranslated())
    		{
    			return;
    		}
    	}
    	
    	doExecute(CompileTranslateExecuter.OPERATION_COMPILE);
    	getActiveTextArea().requestFocus();
	}
	
	private void onOperationTranslateCompile()
	{
    	if (isSourceDirty())
    	{
    		if (!saveSource())
    		{
    			return;
    		}
    	}
    	
    	doExecute(CompileTranslateExecuter.OPERATION_TRANSLATE_AND_COMPILE);
    	getActiveTextArea().requestFocus();
	}
	
	private void onOperationClearResults()
	{
		m_sourceModule.deleteProperties();
	}
	
	private void onOperationExecute()
	{
    	ProgramSpec spec = UserSettings.getExecuterSpec();
    	String strPrefix = null;
    	if (Main.isWindows())
    	{
    		strPrefix = "cmd.exe /c start";
    	}
    	
		String strCommandLine = spec.buildCommand(m_sourceModule, strPrefix);
	
    	try
    	{
	        Runtime.getRuntime().exec(strCommandLine, null, m_sourceModule.getSourceDirectory());
    	}
    	catch(IOException e)
    	{
    	}
    	
    	getActiveTextArea().requestFocus();
	}
	
	private void onHelpAbout()
	{
		DialogAbout.run(this);
    	getActiveTextArea().requestFocus();
	}
	
	private SourceModule	m_sourceModule;
	private JSplitPane		m_splitter;
	private JTabbedPane	m_tabPane;
	private ErrorPane		m_errorPane;
	private Document		m_docSource;
	private Document		m_docTranslated;
	private JTextArea		m_taSource;
	private JTextArea		m_taTranslated;
	private char[]		m_inputBuffer;
	private boolean		m_bSourceDirty;
	private boolean		m_bTranslatedDirty;
	private DialogFind		m_dlgFind;
	
	private class CloseListener extends WindowAdapter
	{
		public void windowClosing(WindowEvent e)
		{
			SourceWindow.this.closeWindow();
		}
	}
	
    private class SourceDocumentListener implements DocumentListener
    {
    	public void changedUpdate(DocumentEvent e)
    	{
    	}
    	public void insertUpdate(DocumentEvent e)
    	{
    		SourceWindow.this.setSourceDirty(true);
    	}
    	public void removeUpdate(DocumentEvent e)
    	{
    		SourceWindow.this.setSourceDirty(true);
    	}
    }

    private class TranslatedDocumentListener implements DocumentListener
    {
    	public void changedUpdate(DocumentEvent e)
    	{
    	}
    	public void insertUpdate(DocumentEvent e)
    	{
    		SourceWindow.this.setTranslatedDirty(true);
    	}
    	public void removeUpdate(DocumentEvent e)
    	{
    		SourceWindow.this.setTranslatedDirty(true);
    	}
    }
    
    private class TabSelectionListener implements ChangeListener
    {
    	public void stateChanged(ChangeEvent e)
    	{
    		SourceWindow.this.getActiveTextArea().requestFocus();
    	}
    }

    private class FileSaveAction extends AbstractAction
    {
        public FileSaveAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.File.Save");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onFileSave();
        }
    }

    private class ButtonSaveAction extends AbstractAction
    {
        public ButtonSaveAction()
        {
            Images.setActionIcon(this, Images.ButtonSave);
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onFileSave();
        }
    }

    private class FileGlobalSettingsAction extends AbstractAction
    {
        public FileGlobalSettingsAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.File.GlobalSettings");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onFileGlobalSettings();
        }
    }

    private class FileDirectorySettingsAction extends AbstractAction
    {
        public FileDirectorySettingsAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.File.DirectorySettings");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onFileDirectorySettings();
        }
    }

    private class FileCloseAction extends AbstractAction
    {
        public FileCloseAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.File.Close");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.closeWindow();
        }
    }

    private class EditCutAction extends AbstractAction
    {
        public EditCutAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.Cut");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditCut();
        }
    }

    private class ButtonCutAction extends AbstractAction
    {
    	public ButtonCutAction()
    	{
    		Images.setActionIcon(this, Images.ButtonCut);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditCut();
        }
    }

    private class EditCopyAction extends AbstractAction
    {
        public EditCopyAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.Copy");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditCopy();
        }
    }

    private class ButtonCopyAction extends AbstractAction
    {
    	public ButtonCopyAction()
    	{
    		Images.setActionIcon(this, Images.ButtonCopy);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditCopy();
        }
    }

    private class EditPasteAction extends AbstractAction
    {
        public EditPasteAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.Paste");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditPaste();
        }
    }

    private class ButtonPasteAction extends AbstractAction
    {
    	public ButtonPasteAction()
    	{
    		Images.setActionIcon(this, Images.ButtonPaste);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditPaste();
        }
    }

    private class EditGotoAction extends AbstractAction
    {
        public EditGotoAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.Goto");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditGoto();
        }
    }

    private class ButtonGotoAction extends AbstractAction
    {
    	public ButtonGotoAction()
    	{
    		Images.setActionIcon(this, Images.ButtonGoTo);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditGoto();
        }
    }

    private class EditFindAction extends AbstractAction
    {
        public EditFindAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.Find");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditFind();
        }
    }
    
    private class ButtonFindAction extends AbstractAction
    {
    	public ButtonFindAction()
    	{
    		Images.setActionIcon(this, Images.ButtonFind);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditFind();
        }
    }

    private class EditFindAgainAction extends AbstractAction
    {
        public EditFindAgainAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Edit.FindAgain");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditFindAgain();
        }
    }
    
    private class ButtonFindAgainAction extends AbstractAction
    {
    	public ButtonFindAgainAction()
    	{
    		Images.setActionIcon(this, Images.ButtonFindAgain);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onEditFindAgain();
        }
    }

	private class OperationTranslateAction extends AbstractAction
    {
        public OperationTranslateAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Operation.Translate");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationTranslate();
        }
    }

    private class ButtonTranslateAction extends AbstractAction
    {
    	public ButtonTranslateAction()
    	{
    		Images.setActionIcon(this, Images.ButtonTranslate);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationTranslate();
        }
    }

    private class OperationCompileAction extends AbstractAction
    {
        public OperationCompileAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Operation.Compile");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationCompile();
        }
    }

    private class ButtonCompileAction extends AbstractAction
    {
    	public ButtonCompileAction()
    	{
    		Images.setActionIcon(this, Images.ButtonCompile);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationCompile();
        }
    }

    private class OperationTranslateCompileAction extends AbstractAction
    {
        public OperationTranslateCompileAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Operation.TranslateCompile");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationTranslateCompile();
        }
    }
    
    private class ButtonTranslateCompileAction extends AbstractAction
    {
    	public ButtonTranslateCompileAction()
    	{
    		Images.setActionIcon(this, Images.ButtonTranComp);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationTranslateCompile();
        }
    }

    private class OperationExecuteAction extends AbstractAction
    {
        public OperationExecuteAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Operation.Execute");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationExecute();
        }
    }
    
    private class ButtonExecuteAction extends AbstractAction
    {
    	public ButtonExecuteAction()
    	{
    		Images.setActionIcon(this, Images.ButtonRun);
    	}

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onOperationExecute();
        }
    }

    private class OperationClearResultsAction extends AbstractAction
    {
        public OperationClearResultsAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Operation.ClearResults");
        }

        public void actionPerformed(ActionEvent event)
        {
			SourceWindow.this.onOperationClearResults();
        }
    }

    private class HelpAboutAction extends AbstractAction
    {
        public HelpAboutAction()
        {
            Text.setActionMenu(this, "SourceWindow.menu.Help.About");
        }

        public void actionPerformed(ActionEvent event)
        {
        	SourceWindow.this.onHelpAbout();
        }
    }

	private EditFindAgainAction	m_actionFindAgain = new EditFindAgainAction();
	private ButtonFindAgainAction	m_buttonFindAgain = new ButtonFindAgainAction();
	private OperationExecuteAction m_actionExecute = new OperationExecuteAction();
	private ButtonExecuteAction	m_buttonExecute = new ButtonExecuteAction();
	
    private Action[] m_fileMenu =
    {
        new FileSaveAction(),
        null,
        new FileGlobalSettingsAction(),
        new FileDirectorySettingsAction(),
        null,
        new FileCloseAction()
    };

    private Action[] m_editMenu =
    {
        new EditCutAction(),
        new EditCopyAction(),
        new EditPasteAction(),
        null,
        new EditGotoAction(),
        null,
        new EditFindAction(),
        m_actionFindAgain
    };

    private Action[] m_operationMenu =
    {
        new OperationTranslateAction(),
        new OperationCompileAction(),
        new OperationTranslateCompileAction(),
        null,
        new OperationClearResultsAction(),
        null,
        m_actionExecute
    };

    private Action[] m_helpMenu =
    {
        new HelpAboutAction()
    };
    
    private static class InsertTabAction extends TextAction
    {
    	public InsertTabAction()
    	{
    		super(DefaultEditorKit.insertTabAction);
    	}
    	
    	public void actionPerformed(ActionEvent event)
    	{
    		JTextArea target = (JTextArea)getTextComponent(event);
    		if (target == null)
    		{
    			return;
    		}
    		
    		if (!target.isEditable() || !target.isEnabled())
    		{
    			Toolkit.getDefaultToolkit().beep();
    			return;
    		}
    		
    		int nCaretPos = target.getCaretPosition();
    		int nSelStart = target.getSelectionStart();
    		int nSelEnd = target.getSelectionEnd();
    		
    		if (nSelStart != nSelEnd)
    		{
    			Toolkit.getDefaultToolkit().beep();
    			return;
    		}
    		
    		try
    		{
	    		int nLineNumber = target.getLineOfOffset(nCaretPos);
	    		int nCurrentColumn = nCaretPos - target.getLineStartOffset(nLineNumber) + 1;
	    		int nNextColumn = -1;
	    		for (int i = 0; i < m_tabStops.length; i++)
	    		{
	    			if (m_tabStops[i] > nCurrentColumn)
	    			{
	    				nNextColumn = m_tabStops[i];
	    				break;
	    			}
	    		}
	    		
	    		if (nNextColumn < 0)
	    		{
	    			Toolkit.getDefaultToolkit().beep();
	    			return;
	    		}
	    		
	    		while(nCurrentColumn < nNextColumn)
	    		{
	    			target.insert(" ", nCaretPos);
	    			nCurrentColumn++;
	    		}
    		}
    		catch(BadLocationException e)
    		{
    		}
    	}
    	
    	private static int[] m_tabStops = { 7,8,12,16,20,24,28,32, 33, 40, 73 };
    }
    
    private static InsertTabAction	m_insertTabAction = new InsertTabAction();
}
