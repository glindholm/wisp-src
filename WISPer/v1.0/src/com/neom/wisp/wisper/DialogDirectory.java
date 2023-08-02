package com.neom.wisp.wisper;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import javax.swing.Box;
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
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogDirectory extends JDialog
{
    public static boolean run(JFrame parent, Arguments args, Arguments globalArgs)
    {
        DialogDirectory dlg = new DialogDirectory(parent);
        dlg.setArguments(args);
        dlg.setGlobalArguments(globalArgs);
        dlg.show();     // blocks
        boolean bOk = dlg.wasOkPressed();
        if (bOk)
        {
            dlg.retrieveArguments(args);
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
        c.add(new JLabel(Text.getString("DialogDirectory.label.TranslatorArgs")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogDirectory.label.CompilerArgs")), constraints);

        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 100.0;

        c.add(m_cbUseGlobal, constraints);
        constraints.gridy++;
        c.add(m_tfTranslatorArgs, constraints);
        constraints.gridy++;
        c.add(m_tfCompilerArgs, constraints);

        constraints.anchor = GridBagConstraints.CENTER;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 0;
        constraints.gridy = 3;
        constraints.gridwidth=2;
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
        if (m_cbUseGlobal.isSelected())
        {
            return(true);
        }

        String strTranslatorArgs = m_tfTranslatorArgs.getText().trim();
        String strCompilerArgs = m_tfCompilerArgs.getText().trim();

        if (strTranslatorArgs.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogDirectory.error.TranslatorArgsRequired"),
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorArgs.requestFocus();
            return(false);
        }

        if (strCompilerArgs.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogDirectory.error.CompilerArgsRequired"),
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerArgs.requestFocus();
            return(false);
        }

        String[] badPart = new String[1];

        ArgumentTemplate.InvalidRange badRange = ArgumentTemplate.findInvalidPortion(strTranslatorArgs);
        if (badRange != null)
        {
            badPart[0] = strTranslatorArgs.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogDirectory.error.TranslatorArgsBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorArgs.setText(strTranslatorArgs);
            m_tfTranslatorArgs.setSelectionStart(badRange.getStart());
            m_tfTranslatorArgs.setSelectionEnd(badRange.getEnd()+1);
            m_tfTranslatorArgs.requestFocus();
            return(false);
        }

        badRange = ArgumentTemplate.findInvalidPortion(strCompilerArgs);
        if (badRange != null)
        {
            badPart[0] = strCompilerArgs.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogDirectory.error.CompilerArgsBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogDirectory.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerArgs.setText(strCompilerArgs);
            m_tfCompilerArgs.setSelectionStart(badRange.getStart());
            m_tfCompilerArgs.setSelectionEnd(badRange.getEnd()+1);
            m_tfCompilerArgs.requestFocus();
            return(false);
        }

        return(true);
    }

    public void setArguments(Arguments args)
    {
        m_cbUseGlobal.setSelected(args.getUseGlobal());
        m_tfTranslatorArgs.setText(args.getTranslatorArgs());
        m_tfCompilerArgs.setText(args.getCompilerArgs());
        updateFields();
    }

    public void setGlobalArguments(Arguments args)
    {
        m_globalArgs = args.copy();
    }

    public void retrieveArguments(Arguments args)
    {
        if (m_cbUseGlobal.isSelected())
        {
            args.setUseGlobal(true);
            args.setTranslatorArgs("");
            args.setCompilerArgs("");
        }
        else
        {
            args.setUseGlobal(false);
            args.setTranslatorArgs(m_tfTranslatorArgs.getText().trim());
            args.setCompilerArgs( m_tfCompilerArgs.getText().trim());
        }
    }

    private void updateFields()
    {
        if (m_cbUseGlobal.isSelected())
        {
            m_tfTranslatorArgs.setEnabled(false);
            m_tfCompilerArgs.setEnabled(false);
        }
        else
        {
            if (m_tfTranslatorArgs.getText().trim().length() == 0)
            {
                if (m_globalArgs != null)
                {
                    m_tfTranslatorArgs.setText(m_globalArgs.getTranslatorArgs());
                }
            }
            m_tfTranslatorArgs.setEnabled(true);
            if (m_tfCompilerArgs.getText().trim().length() == 0)
            {
                if (m_globalArgs != null)
                {
                    m_tfCompilerArgs.setText(m_globalArgs.getCompilerArgs());
                }
            }
            m_tfCompilerArgs.setEnabled(true);
        }
    }

    private class OkButtonAction extends WispAction
    {
        public OkButtonAction()
        {
            super("DialogDirectory.button.Ok");
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

    private class CancelButtonAction extends WispAction
    {
        public CancelButtonAction()
        {
            super("DialogDirectory.button.Cancel");
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

    private JCheckBox   m_cbUseGlobal       = new JCheckBox();
    private JTextField  m_tfTranslatorArgs  = new JTextField(36);
    private JTextField  m_tfCompilerArgs    = new JTextField(36);
    private Arguments   m_globalArgs;
    private boolean     m_bOkPressed = false;
}
