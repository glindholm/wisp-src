package com.neom.wisp.wisper.gui;
import com.neom.wisp.wisper.*;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import java.awt.Container;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import java.io.File;
import javax.swing.JOptionPane;
import java.text.MessageFormat;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogGlobals extends JDialog
{
    public static boolean run(JFrame parent,
    							ProgramSpec specTranslator, 
    							ProgramSpec specCompiler,
    							ProgramSpec specExecuter)
    {
        DialogGlobals dlg = new DialogGlobals(parent);
        dlg.setTranslator(specTranslator);
        dlg.setCompiler(specCompiler);
        dlg.setExecuter(specExecuter);
        dlg.show();     // blocks
        boolean bOk = dlg.wasOkPressed();
        if (bOk)
        {
            dlg.retrieveTranslator(specTranslator);
            dlg.retrieveCompiler(specCompiler);
            dlg.retrieveExecuter(specExecuter);
        }
        dlg.dispose();
        return(bOk);
    }

    public DialogGlobals(JFrame parent)
    {
        super(parent, Text.getString("DialogGlobals.title"), true);
        setName("DialogGlobals");

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(new JButton(new OkButtonAction()));
        buttonPanel.add(new JButton(new CancelButtonAction()));

        GridBagLayout layout = new GridBagLayout();
        Insets insets = new Insets(5,5,5,5);
        GridBagConstraints constraints = new GridBagConstraints(0,0,    // gridx, gridy
                                                                1,1,    // gridwidth, gridheight
                                                                0.0,0.0,// weightx, weighty
                                                                GridBagConstraints.EAST,//anchor
                                                                GridBagConstraints.NONE,    // fill
                                                                insets,
                                                                0,0);   // ipadx, ipady

        Container c = getContentPane();
        c.setLayout(layout);

        c.add(new JLabel(Text.getString("DialogGlobals.label.Translator")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.TranslatorTemplate")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.Compiler")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.CompilerTemplate")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.ExecuterTemplate")), constraints);

        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 100.0;

        c.add(m_tfTranslator, constraints);

        constraints.gridy++;
        constraints.gridwidth=2;
        c.add(m_tfTranslatorTemplate, constraints);

        constraints.gridy++;
        constraints.gridwidth=1;
        c.add(m_tfCompiler, constraints);

        constraints.gridy++;
        constraints.gridwidth=2;
        c.add(m_tfCompilerTemplate, constraints);

        constraints.gridy++;
        constraints.gridwidth=2;
        c.add(m_tfExecuterTemplate, constraints);

        constraints.gridx = 2;
        constraints.gridy = 0;
        constraints.gridwidth = 1;
        constraints.weightx = 0.0;
        c.add(new JButton(new BrowseTranslatorButtonAction()), constraints);

        constraints.gridy += 2;
        c.add(new JButton(new BrowseCompilerButtonAction()), constraints);

        constraints.anchor = GridBagConstraints.CENTER;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 6;
        constraints.gridwidth=3;
        c.add(buttonPanel, constraints);

        pack();

        Main.setMinSize(this);
        Main.centerInScreen(this);
    }

    public boolean wasOkPressed()
    {
        return(m_bOkPressed);
    }

    public boolean isValid()
    {
        String strTranslator = m_tfTranslator.getText().trim();
        String strTranslatorTemplate = m_tfTranslatorTemplate.getText().trim();
        String strCompiler = m_tfCompiler.getText().trim();
        String strCompilerTemplate = m_tfCompilerTemplate.getText().trim();
        String strExecuterTemplate = m_tfExecuterTemplate.getText().trim();

        if (strTranslator.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.TranslatorRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslator.requestFocus();
            return(false);
        }
        
        File translatorExe = new File(strTranslator);
        if (!translatorExe.exists())
        {
        	String template = Text.getString("DialogGlobals.error.ExeMissing");
	        String[] args = new String[1];
	        args[0] = strTranslator;
	        String message = MessageFormat.format(template, args);
        	
            JOptionPane.showMessageDialog(  this,
                                            message,
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslator.requestFocus();
            return(false);
        }

        if (strTranslatorTemplate.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.TranslatorTemplateRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorTemplate.requestFocus();
            return(false);
        }

        if (strCompiler.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.CompilerRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompiler.requestFocus();
            return(false);
        }

        File compilerExe = new File(strCompiler);
        if (!compilerExe.exists())
        {
        	String template = Text.getString("DialogGlobals.error.ExeMissing");
	        String[] args = new String[1];
	        args[0] = strCompiler;
	        String message = MessageFormat.format(template, args);
        	
            JOptionPane.showMessageDialog(  this,
                                            message,
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslator.requestFocus();
            return(false);
        }

        if (strCompilerTemplate.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.CompilerTemplateRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerTemplate.requestFocus();
            return(false);
        }

        String[] badPart = new String[1];

        ProgramSpec.InvalidRange badRange = ProgramSpec.validateArgumentTemplate(strTranslatorTemplate);
        if (badRange != null)
        {
            badPart[0] = strTranslatorTemplate.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogGlobals.error.TranslatorTemplateBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorTemplate.setText(strTranslatorTemplate);
            m_tfTranslatorTemplate.setSelectionStart(badRange.getStart());
            m_tfTranslatorTemplate.setSelectionEnd(badRange.getEnd()+1);
            m_tfTranslatorTemplate.requestFocus();
            return(false);
        }

        badRange = ProgramSpec.validateArgumentTemplate(strCompilerTemplate);
        if (badRange != null)
        {
            badPart[0] = strCompilerTemplate.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogGlobals.error.CompilerTemplateBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerTemplate.setText(strCompilerTemplate);
            m_tfCompilerTemplate.setSelectionStart(badRange.getStart());
            m_tfCompilerTemplate.setSelectionEnd(badRange.getEnd()+1);
            m_tfCompilerTemplate.requestFocus();
            return(false);
        }

        if (strExecuterTemplate.length() > 0)
        {
	        badRange = ProgramSpec.validateArgumentTemplate(strExecuterTemplate);
	        if (badRange != null)
	        {
	            badPart[0] = strExecuterTemplate.substring(badRange.getStart(), badRange.getEnd()+1);
	            String msg = MessageFormat.format(Text.getString("DialogGlobals.error.ExecuterTemplateBad"), badPart);
	            JOptionPane.showMessageDialog(  this,
	                                            msg,
	                                            Text.getString("DialogGlobals.error.title"),
	                                            JOptionPane.ERROR_MESSAGE);
	            m_tfExecuterTemplate.setText(strExecuterTemplate);
	            m_tfExecuterTemplate.setSelectionStart(badRange.getStart());
	            m_tfExecuterTemplate.setSelectionEnd(badRange.getEnd()+1);
	            m_tfExecuterTemplate.requestFocus();
	            return(false);
	        }
        }

        return(true);
    }

    public void setTranslator(ProgramSpec specTranslator)
    {
        m_tfTranslator.setText(specTranslator.getExecutable());
        m_tfTranslatorTemplate.setText(specTranslator.getArgumentTemplate());
    }

    public void retrieveTranslator(ProgramSpec specTranslator)
    {
        specTranslator.setExecutable(m_tfTranslator.getText().trim());
        specTranslator.setArgumentTemplate(m_tfTranslatorTemplate.getText().trim());
    }

    public void setCompiler(ProgramSpec specCompiler)
    {
        m_tfCompiler.setText(specCompiler.getExecutable());
        m_tfCompilerTemplate.setText(specCompiler.getArgumentTemplate());
    }

    public void retrieveCompiler(ProgramSpec specCompiler)
    {
        specCompiler.setExecutable(m_tfCompiler.getText().trim());
        specCompiler.setArgumentTemplate(m_tfCompilerTemplate.getText().trim());
    }

    public void setExecuter(ProgramSpec specExecuter)
    {
        m_tfExecuterTemplate.setText(specExecuter.getArgumentTemplate());
    }

    public void retrieveExecuter(ProgramSpec specExecuter)
    {
        specExecuter.setExecutable("");
        specExecuter.setArgumentTemplate(m_tfExecuterTemplate.getText().trim());
    }

    private class OkButtonAction extends AbstractAction
    {
        public OkButtonAction()
        {
            Text.setActionMenu(this, "DialogGlobals.button.Ok");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (DialogGlobals.this.isValid())
            {
                DialogGlobals.this.m_bOkPressed = true;
                DialogGlobals.this.hide();
            }
        }
    }

    private class CancelButtonAction extends AbstractAction
    {
        public CancelButtonAction()
        {
            Text.setActionMenu(this, "DialogGlobals.button.Cancel");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogGlobals.this.hide();
        }
    }

    private abstract class BrowseAction extends AbstractAction
    {
        public BrowseAction(String s)
        {
            Text.setActionMenu(this, s);
        }

        protected String doBrowse(String title, String initialValue)
        {
            JFileChooser chooser = new JFileChooser();

            if (Main.isWindows())
            {
                chooser.setFileFilter(new ExeFileFilter());
            }

            chooser.setDialogTitle(title);
            Text.MenuInfo info = Text.getMenuInfo("DialogGlobals.filechooser.button.Select");
            chooser.setApproveButtonText(info.getName());
            chooser.setApproveButtonMnemonic(info.getMnemonic().intValue());

            if (initialValue != null && initialValue.length() > 0)
            {
                File initialFile = new File(initialValue);
                if (initialFile.exists())
                {
                    chooser.setSelectedFile(initialFile);
                }
                else
                {
                    File initialDir = initialFile.getParentFile();
                    if (initialDir != null && initialDir.exists())
                    {
                        chooser.setCurrentDirectory(initialDir);
                    }
                }
            }

            int nResult = chooser.showDialog(DialogGlobals.this, Text.getString("DialogGlobals.button.Select"));
            if (nResult != JFileChooser.APPROVE_OPTION)
            {
                return(null);
            }

            return(chooser.getSelectedFile().getAbsolutePath());
        }
    }

    private class BrowseTranslatorButtonAction extends BrowseAction
    {
        public BrowseTranslatorButtonAction()
        {
            super("DialogGlobals.button.Browse");
        }

        public void actionPerformed(ActionEvent event)
        {
            String strTranslator = super.doBrowse(  Text.getString("DialogGlobals.filechooser.title.Translator"),
                                                    m_tfTranslator.getText());
            if (strTranslator != null)
            {
                m_tfTranslator.setText(strTranslator);
            }
        }
    }

    private class BrowseCompilerButtonAction extends BrowseAction
    {
        public BrowseCompilerButtonAction()
        {
            super("DialogGlobals.button.Browse");
        }

        public void actionPerformed(ActionEvent event)
        {
            String strCompiler = super.doBrowse(Text.getString("DialogGlobals.filechooser.title.Compiler"),
                                                m_tfCompiler.getText());
            if (strCompiler != null)
            {
                m_tfCompiler.setText(strCompiler);
            }
        }
    }

    private class ExeFileFilter extends FileFilter
    {
        public ExeFileFilter()
        {
        }

        public boolean accept(File f)
        {
            if (f.isDirectory())
            {
                return(true);
            }

            String name = f.getName();
            if (name.length() < 4)
            {
                return(false);
            }
            String extension = name.substring(name.length()-4);
            if (extension.compareToIgnoreCase(".exe") == 0)
            {
                return(true);
            }

            return(false);
        }

        public String getDescription()
        {
            return(Text.getString("DialogGlobals.filefilter.description"));
        }
    }

    private JTextField m_tfTranslator      	= new JTextField(24);
    private JTextField m_tfTranslatorTemplate	= new JTextField(36);
    private JTextField m_tfCompiler        	= new JTextField(24);
    private JTextField m_tfCompilerTemplate	= new JTextField(36);
    private JTextField m_tfExecuterTemplate	= new JTextField(36);
    private boolean	m_bOkPressed = false;
}
