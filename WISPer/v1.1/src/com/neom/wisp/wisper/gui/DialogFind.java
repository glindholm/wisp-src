package com.neom.wisp.wisper.gui;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

/**
 * @author Kevin Hunter
 *
 * Copyright (c) Shell Stream Software LLC, All Rights Reserved.
 */
public class DialogFind extends JDialog
{
    public DialogFind(JFrame parent)
    {
        super(parent, Text.getString("DialogFind.title"), true);
        setName("DialogFind");
        
        m_actionFind = new FindAction();
        
        m_tfString = new JTextField(30);
        m_tfString.getDocument().addDocumentListener(new SearchTextListener());
        m_cbIgnoreCase = new JCheckBox(Text.getString("DialogFind.label.ignoreCase"));
        m_cbIgnoreCase.setSelected(true);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout());
        
        JButton findButton = new JButton(m_actionFind);
        getRootPane().setDefaultButton(findButton);
        
        buttonPanel.add(findButton);
        buttonPanel.add(new JButton(new CancelAction()));
        
        GridBagLayout layout = new GridBagLayout();
        Insets insets = new Insets(5,5,5,5);
        GridBagConstraints constraints = new GridBagConstraints(0,0,    // gridx, gridy
                                                                1,1,    // gridwidth, gridheight
                                                                33.0,0.0,// weightx, weighty
                                                                GridBagConstraints.EAST,//anchor
                                                                GridBagConstraints.NONE,    // fill
                                                                insets,
                                                                0,0);   // ipadx, ipady

        Container c = getContentPane();
        c.setLayout(layout);

        c.add(new JLabel(Text.getString("DialogFind.label.text")), constraints);
        
        constraints.gridx = 1;
        constraints.weightx = 100.0;
        constraints.anchor = GridBagConstraints.WEST;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        
        c.add(m_tfString, constraints);
        
        constraints.gridx = 0;
        constraints.gridy = 1;
        constraints.fill = GridBagConstraints.NONE;
        constraints.anchor = GridBagConstraints.CENTER;
        constraints.weightx = 0.0;
        constraints.gridwidth = 2;
        
        c.add(m_cbIgnoreCase, constraints);

		constraints.gridy = 2;        
        
        c.add(buttonPanel, constraints);
        
        pack();

        Main.setMinSize(this);
        Main.centerInScreen(this);
    }
    
    public void setFindText(String text)
    {
    	m_tfString.setText(text);
    }
    
    public String getFindText()
    {
    	return(m_tfString.getText());
    }
    
    public boolean getIgnoreCase()
    {
    	return(m_cbIgnoreCase.isSelected());
    }
    
    public boolean run()
    {
    	m_bOkPressed = false;
        m_actionFind.updateState();
        
		m_tfString.setSelectionStart(0);
    	m_tfString.setSelectionEnd(m_tfString.getText().length());
    	m_tfString.requestFocus();

    	show();	// blocks
    	return(m_bOkPressed);
    }
    
    private boolean	m_bOkPressed;
    private FindAction	m_actionFind;
    private JTextField	m_tfString;
    private JCheckBox	m_cbIgnoreCase;
    
    private class FindAction extends AbstractAction
    {
    	public FindAction()
    	{
    		Text.setActionMenu(this, "DialogFind.button.Find");
    	}
    	
    	public void actionPerformed(ActionEvent event)
    	{
    		m_bOkPressed = true;
    		hide();
    	}
    	
    	public void updateState()
    	{
    		if (m_tfString.getText().length() == 0)
    		{
    			setEnabled(false);
    		}
    		else
    		{
    			setEnabled(true);
    		}
    	}
    }
    
    private class CancelAction extends AbstractAction
    {
    	public CancelAction()
    	{
    		Text.setActionMenu(this, "DialogFind.button.Cancel");
    	}
    	
    	public void actionPerformed(ActionEvent event)
    	{
    		m_bOkPressed = false;
    		hide();
    	}
    }
    
    private class SearchTextListener implements DocumentListener
    {
    	public void changedUpdate(DocumentEvent e)
    	{
    	}
    	public void insertUpdate(DocumentEvent e)
    	{
    		m_actionFind.setEnabled(true);
    	}
    	public void removeUpdate(DocumentEvent e)
    	{
    		m_actionFind.updateState();
    	}
    } 
}
