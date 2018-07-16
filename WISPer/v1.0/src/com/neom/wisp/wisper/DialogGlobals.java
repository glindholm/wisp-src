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

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogGlobals extends JDialog
{
    public static boolean run(JFrame parent, Executables exes, Arguments args)
    {
        DialogGlobals dlg = new DialogGlobals(parent);
        dlg.setExecutables(exes);
        dlg.setArguments(args);
        dlg.show();     // blocks
        boolean bOk = dlg.wasOkPressed();
        if (bOk)
        {
            dlg.retrieveExecutables(exes);
            dlg.retrieveArguments(args);
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
        c.add(new JLabel(Text.getString("DialogGlobals.label.TranslatorArgs")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.Compiler")), constraints);
        constraints.gridy++;
        c.add(new JLabel(Text.getString("DialogGlobals.label.CompilerArgs")), constraints);

        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.gridx = 1;
        constraints.gridy = 0;
        constraints.weightx = 100.0;

        c.add(m_tfTranslator, constraints);

        constraints.gridy++;
        constraints.gridwidth=2;
        c.add(m_tfTranslatorArgs, constraints);

        constraints.gridy++;
        constraints.gridwidth=1;
        c.add(m_tfCompiler, constraints);

        constraints.gridy++;
        constraints.gridwidth=2;
        c.add(m_tfCompilerArgs, constraints);

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
        constraints.gridy = 4;
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
        String strTranslatorArgs = m_tfTranslatorArgs.getText().trim();
        String strCompiler = m_tfCompiler.getText().trim();
        String strCompilerArgs = m_tfCompilerArgs.getText().trim();

        if (strTranslator.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.TranslatorRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslator.requestFocus();
            return(false);
        }

        if (strTranslatorArgs.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.TranslatorArgsRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfTranslatorArgs.requestFocus();
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

        if (strCompilerArgs.length() == 0)
        {
            JOptionPane.showMessageDialog(  this,
                                            Text.getString("DialogGlobals.error.CompilerArgsRequired"),
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerArgs.requestFocus();
            return(false);
        }

        String[] badPart = new String[1];

        ArgumentTemplate.InvalidRange badRange = ArgumentTemplate.findInvalidPortion(strTranslatorArgs);
        if (badRange != null)
        {
            badPart[0] = strTranslatorArgs.substring(badRange.getStart(), badRange.getEnd()+1);
            String msg = MessageFormat.format(Text.getString("DialogGlobals.error.TranslatorArgsBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogGlobals.error.title"),
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
            String msg = MessageFormat.format(Text.getString("DialogGlobals.error.CompilerArgsBad"), badPart);
            JOptionPane.showMessageDialog(  this,
                                            msg,
                                            Text.getString("DialogGlobals.error.title"),
                                            JOptionPane.ERROR_MESSAGE);
            m_tfCompilerArgs.setText(strCompilerArgs);
            m_tfCompilerArgs.setSelectionStart(badRange.getStart());
            m_tfCompilerArgs.setSelectionEnd(badRange.getEnd()+1);
            m_tfCompilerArgs.requestFocus();
            return(false);
        }

        return(true);
    }

    public void setExecutables(Executables exes)
    {
        m_tfTranslator.setText(exes.getTranslator());
        m_tfCompiler.setText(exes.getCompiler());
    }

    public void retrieveExecutables(Executables exes)
    {
        exes.setTranslator(m_tfTranslator.getText().trim());
        exes.setCompiler(m_tfCompiler.getText().trim());
    }

    public void setArguments(Arguments args)
    {
        m_tfTranslatorArgs.setText(args.getTranslatorArgs());
        m_tfCompilerArgs.setText(args.getCompilerArgs());
    }

    public void retrieveArguments(Arguments args)
    {
        args.setTranslatorArgs(m_tfTranslatorArgs.getText().trim());
        args.setCompilerArgs( m_tfCompilerArgs.getText().trim());
    }

    private class OkButtonAction extends WispAction
    {
        public OkButtonAction()
        {
            super("DialogGlobals.button.Ok");
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

    private class CancelButtonAction extends WispAction
    {
        public CancelButtonAction()
        {
            super("DialogGlobals.button.Cancel");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogGlobals.this.hide();
        }
    }

    private abstract class BrowseAction extends WispAction
    {
        public BrowseAction(String s)
        {
            super(s);
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

    private JTextField  m_tfTranslator      = new JTextField(24);
    private JTextField  m_tfTranslatorArgs  = new JTextField(36);
    private JTextField  m_tfCompiler        = new JTextField(24);
    private JTextField  m_tfCompilerArgs    = new JTextField(36);
    private boolean     m_bOkPressed = false;
}