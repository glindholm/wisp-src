package com.neom.wisp.wisper.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class DialogProgress extends JDialog
{
    public DialogProgress(Frame parent)
    {
        super(parent, Text.getString("DialogProgress.title"), true);
        setName("DialogProgress");

        setResizable(false);

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

        m_progress.setValue(0);

        Container container = getContentPane();
        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints constraint = new GridBagConstraints();

        container.setLayout(layout);

        constraint.gridheight = 1;
        constraint.gridwidth = 1;
        constraint.weightx = 100.;
        constraint.weighty = 100.;
        constraint.anchor = GridBagConstraints.CENTER;
        constraint.insets = new Insets(0, 5, 0, 5);

        addItem(container, layout, constraint, 0, 0, m_label);
        constraint.fill = GridBagConstraints.HORIZONTAL;
        addItem(container, layout, constraint, 0, 1, m_progress);
        constraint.fill = GridBagConstraints.NONE;
        addItem(container, layout, constraint, 0, 2, m_buttonCancel);

        pack();
        Main.setMinSize(this);
        Main.centerInScreen(this);
    }

    public synchronized boolean wasCancelled()
    {
        return(m_bCancelled);
    }

    public synchronized void setCancelled(boolean bCancelled)
    {
        m_bCancelled = bCancelled;
        if (m_bCancelled && m_process != null)
        {
        	m_process.destroy();
        }
    }
    
    public synchronized void setProcess(Process p)
    {
    	m_process = p;
    }

    public void setMaximum(int value)
    {
        SwingUtilities.invokeLater(new MaximumProxy(value));
    }

    public void setCurrent(int value)
    {
        SwingUtilities.invokeLater(new CurrentProxy(value));
    }

    public void tickCurrent()
    {
        SwingUtilities.invokeLater(new TickProxy());
    }

    public void setLabel(String label)
    {
        if (!m_bCancelled)
        {
            SwingUtilities.invokeLater(new LabelProxy(label));
        }
    }

    public void setLabelTranslating(String fileName)
    {
        String[] args = { fileName };
        setLabel(Text.getMessage("DialogProgress.msg.Translating", args));
    }

    public void setLabelCompiling(String fileName)
    {
        String[] args = { fileName };
        setLabel(Text.getMessage("DialogProgress.msg.Compiling", args));
    }

    public void setLabelClearing(String fileName)
    {
        String[] args = { fileName };
        setLabel(Text.getMessage("DialogProgress.msg.Clearing", args));
    }

    public void doHide()
    {
        SwingUtilities.invokeLater(new HideProxy());
    }

    private void addItem(Container container,
                         GridBagLayout layout,
                         GridBagConstraints constraint,
                         int x,
                         int y,
                         Component item)
    {
        constraint.gridx = x;
        constraint.gridy = y;
        layout.setConstraints(item, constraint);
        container.add(item);
    }

    private boolean		m_bCancelled = false;
    private CancelAction	m_actionCancel = new CancelAction();
    private Process		m_process;

    private JLabel			m_label = new JLabel(" ");
    private JProgressBar	m_progress = new JProgressBar(JProgressBar.HORIZONTAL, 0, 100);
    private JButton		m_buttonCancel = new JButton(m_actionCancel);
    
    private class CancelAction extends AbstractAction
    {
        public CancelAction()
        {
            Text.setActionMenu(this, "DialogProgress.button.Cancel");
            setEnabled(true);
        }

        public void actionPerformed(ActionEvent e)
        {
            setEnabled(false);
            setCancelled(true);
            DialogProgress.this.m_label.setText(Text.getString("DialogProgress.msg.Cancelling"));
        }
    }

    private class MaximumProxy implements Runnable
    {
        public MaximumProxy(int value)
        {
            m_nValue = value;
        }

        public void run()
        {
            DialogProgress.this.m_progress.setMaximum(m_nValue);
        }

        private int m_nValue;
    }

    private class CurrentProxy implements Runnable
    {
        public CurrentProxy(int value)
        {
            m_nValue = value;
        }

        public void run()
        {
            DialogProgress.this.m_progress.setValue(m_nValue);
        }

        private int m_nValue;
    }

    private class TickProxy implements Runnable
    {
        public TickProxy()
        {
        }

        public void run()
        {
            int nValue = DialogProgress.this.m_progress.getValue();
            DialogProgress.this.m_progress.setValue(nValue+1);
        }
    }

    private class LabelProxy implements Runnable
    {
        public LabelProxy(String label)
        {
            m_strLabel = label;
        }

        public void run()
        {
            DialogProgress.this.m_label.setText(m_strLabel);
        }

        private String  m_strLabel;
    }

    private class HideProxy implements Runnable
    {
        public HideProxy()
        {
        }

        public void run()
        {
            DialogProgress.this.hide();
        }
    }
}