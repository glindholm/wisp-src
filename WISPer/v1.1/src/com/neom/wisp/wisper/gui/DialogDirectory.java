package com.neom.wisp.wisper.gui;
import com.neom.wisp.wisper.*;

import java.awt.Color;
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
import javax.swing.JCheckBox;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogDirectory extends JDialog
{
    public static boolean run(JFrame parent,
    							DirectoryOptions options, 
    							ProgramSpec specTranslator, 
    							ProgramSpec specCompiler)
    {
        DialogDirectory dlg = new DialogDirectory(parent);
        dlg.setTranslatorTemplate(specTranslator.getArgumentTemplate());
        dlg.setCompilerTemplate(specCompiler.getArgumentTemplate());
        dlg.setOptions(options);
        dlg.show();     // blocks
        boolean bOk = dlg.wasOkPressed();
        if (bOk)
        {
            dlg.retrieveOptions(options);
        }
        dlg.dispose();
        return(bOk);
    }

    public DialogDirectory(JFrame parent)
    {
        super(parent, Text.getString("DialogDirectory.title"), true);
        setName("DialogDirectory");

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(new JButton(new OkButtonAction()));
        buttonPanel.add(new JButton(new CancelButtonAction()));

        m_cbUseGlobal.addItemListener(new CheckboxListener());

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

        c.add(new JLabel(Text.getString("DialogDirectory.label.UseGlobal")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogDirectory.label.TranslatorTemplate")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogDirectory.label.CompilerTemplate")), constraints);

        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 100.0;

        c.add(m_cbUseGlobal, constraints);
        constraints.gridy++;
        c.add(m_tfTranslatorTemplate, constraints);
        constraints.gridy++;
        c.add(m_tfCompilerTemplate, constraints);

        constraints.anchor = GridBagConstraints.CENTER;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth=2;
        c.add(buttonPanel, constraints);
        
        m_clrEnabled = m_tfTranslatorTemplate.getBackground();
        m_clrDisabled = getBackground();

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
        if (m_cbUseGlobal.isSelected())
        {
            return(true);
        }

        String strTranslatorTemplate = m_tfTranslatorTemplate.getText().trim();
        String strCompilerTemplate = m_tfCompilerTemplate.getText().trim();

        if (strTranslatorTemplate.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogDirectory.error.TranslatorTemplateRequired"),
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorTemplate.requestFocus();
            return(false);
        }

        if (strCompilerTemplate.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogDirectory.error.CompilerTemplateRequired"),
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerTemplate.requestFocus();
            return(false);
        }

        String[] badPart = new String[1];

        ProgramSpec.InvalidRange badRange = ProgramSpec.validateArgumentTemplate(strTranslatorTemplate);
        if (badRange != null)
        {
            badPart[0] = strTranslatorTemplate.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogDirectory.error.TranslatorTemplateBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogDirectory.error.title"),
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
            String msg = MessageFormat.format(Text.getString("DialogDirectory.error.CompilerTemplateBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerTemplate.setText(strCompilerTemplate);
            m_tfCompilerTemplate.setSelectionStart(badRange.getStart());
            m_tfCompilerTemplate.setSelectionEnd(badRange.getEnd()+1);
            m_tfCompilerTemplate.requestFocus();
            return(false);
        }

        return(true);
    }

	public void setTranslatorTemplate(String strTranslatorTemplate)
	{
		m_strTranslatorTemplate = strTranslatorTemplate;
	}
	
	public void setCompilerTemplate(String strCompilerTemplate)
	{
		m_strCompilerTemplate = strCompilerTemplate;
	}
	
    public void setOptions(DirectoryOptions options)
    {
        m_cbUseGlobal.setSelected(options.getUseGlobalTemplates());
        
        String strTranslatorTemplate = options.getTranslatorTemplate();
        if (strTranslatorTemplate != null)
        {
        	m_strTranslatorTemplate = strTranslatorTemplate;
        }
        if (m_strTranslatorTemplate != null)
        {
        	m_tfTranslatorTemplate.setText(m_strTranslatorTemplate);
        }
        
        String strCompilerTemplate = options.getCompilerTemplate();
        if (strCompilerTemplate != null)
        {
        	m_strCompilerTemplate = strCompilerTemplate;
        }
        if (m_strCompilerTemplate != null)
        {
        	m_tfCompilerTemplate.setText(m_strCompilerTemplate);
        }
        
        updateFields();
    }

    public void retrieveOptions(DirectoryOptions options)
    {
        options.setUseGlobalTemplates(m_cbUseGlobal.isSelected());
        options.setTranslatorTemplate(m_tfTranslatorTemplate.getText().trim());
        options.setCompilerTemplate( m_tfCompilerTemplate.getText().trim());
    }

    private void updateFields()
    {
        if (m_cbUseGlobal.isSelected())
        {
            m_tfTranslatorTemplate.setEnabled(false);
            m_tfTranslatorTemplate.setBackground(m_clrDisabled);
            m_tfCompilerTemplate.setEnabled(false);
            m_tfCompilerTemplate.setBackground(m_clrDisabled);
        }
        else
        {
            m_tfTranslatorTemplate.setBackground(m_clrEnabled);
            m_tfTranslatorTemplate.setEnabled(true);
            m_tfCompilerTemplate.setBackground(m_clrEnabled);
            m_tfCompilerTemplate.setEnabled(true);
        }
    }

    private class OkButtonAction extends AbstractAction
    {
        public OkButtonAction()
        {
            Text.setActionMenu(this, "DialogDirectory.button.Ok");
        }

        public void actionPerformed(ActionEvent event)
        {
            if (DialogDirectory.this.isValid())
            {
                DialogDirectory.this.m_bOkPressed = true;
                DialogDirectory.this.hide();
            }
        }
    }

    private class CancelButtonAction extends AbstractAction
    {
        public CancelButtonAction()
        {
            Text.setActionMenu(this, "DialogDirectory.button.Cancel");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogDirectory.this.hide();
        }
    }

    private class CheckboxListener implements ItemListener
    {
        public CheckboxListener()
        {
        }

        public void itemStateChanged(ItemEvent e)
        {
            DialogDirectory.this.updateFields();
        }
    }

    private JCheckBox	m_cbUseGlobal			= new JCheckBox();
    private JTextField	m_tfTranslatorTemplate	= new JTextField(36);
    private JTextField	m_tfCompilerTemplate	= new JTextField(36);
    private boolean	m_bOkPressed			= false;
    private Color		m_clrEnabled;
    private Color		m_clrDisabled;
    private String		m_strTranslatorTemplate;
    private String		m_strCompilerTemplate;
}
