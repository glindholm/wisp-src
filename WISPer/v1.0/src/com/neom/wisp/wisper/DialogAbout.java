package com.neom.wisp.wisper;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import java.text.MessageFormat;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogAbout extends JDialog
{
    public static void run(JFrame parent)
    {
        DialogAbout dlg = new DialogAbout(parent);
        dlg.show(); // blocks
        dlg.dispose();
    }

    public DialogAbout(JFrame parent)
    {
        super(parent, Text.getString("DialogAbout.title"), true);
        setName("DialogAbout");

        JLabel icon = new JLabel(Images.getIcon(Images.WISPerIconLarge));

        String[] arguments = new String[1];
        arguments[0] = Text.getString("App.version");
        String textTemplate = Text.getString("DialogAbout.text");
        String textString = MessageFormat.format(textTemplate, arguments);

        JTextArea text = new JTextArea(textString);
        text.setEditable(false);
        text.setBackground((Color)UIManager.get("Label.background"));
        text.setForeground((Color)UIManager.get("Button.foreground"));
        text.setFont((Font)UIManager.get("Label.font"));

        JButton closeButton = new JButton(new CloseButtonAction());
        JPanel panel = new JPanel();
        panel.add(closeButton);

        GridBagLayout layout = new GridBagLayout();
        Insets insets = new Insets(5,5,5,5);
        GridBagConstraints constraints = new GridBagConstraints(0,0,    // gridx, gridy
                                                                1,1,    // gridwidth, gridheight
                                                                0.0,0.0,// weightx, weighty
                                                                GridBagConstraints.NORTHWEST,//anchor
                                                                GridBagConstraints.NONE,    // fill
                                                                insets,
                                                                0,0);   // ipadx, ipady

        Container c = getContentPane();
        c.setLayout(layout);

        c.add(icon, constraints);

        constraints.gridx = 1;
        constraints.weightx = 100.0;
        constraints.gridheight = 2;

        c.add(text, constraints);

        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.gridheight = 1;
        constraints.weightx = 0.0;
        constraints.weighty = 1.0;

        c.add(new JLabel(), constraints);

        constraints.gridy = 2;
        constraints.gridwidth = 2;
        constraints.weighty = 0.0;
        constraints.anchor = GridBagConstraints.CENTER;

        c.add(panel, constraints);

        pack();

        Main.setMinSize(this);
        Main.centerInScreen(this);
    }

    private class CloseButtonAction extends WispAction
    {
        public CloseButtonAction()
        {
            super("DialogAbout.closeButton");
        }

        public void actionPerformed(ActionEvent event)
        {
            DialogAbout.this.hide();
        }
    }
}