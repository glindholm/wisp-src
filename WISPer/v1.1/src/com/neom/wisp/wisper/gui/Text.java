package com.neom.wisp.wisper.gui;
import java.text.*;

import java.util.*;
import javax.swing.*;
import java.awt.event.*;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class Text
{
    public static class MenuInfo
    {
        public MenuInfo(String strText)
        {
            int nCommaPos = strText.indexOf(',');
            if (nCommaPos < 0)
            {
                m_strName = strText;
                return;
            }
            
            m_strName = strText.substring(0, nCommaPos);
            if (nCommaPos >= strText.length() - 1)
            {
            	return;
            }
            
            strText = strText.substring(nCommaPos+1, strText.length());
			m_intMnemonic = new Integer((int)strText.charAt(0));
			nCommaPos = strText.indexOf(',');
			if (nCommaPos > 0 && nCommaPos < strText.length() - 1)
			{
				strText = strText.substring(nCommaPos + 1, strText.length());
				
				if (strText.length() == 1)
				{
					m_ksAccelerator = KeyStroke.getKeyStroke(strText.charAt(0), InputEvent.CTRL_MASK);
				}
				else
				{
					m_ksAccelerator = KeyStroke.getKeyStroke(strText);
				}
			}
        }

        public String getName()
        {
            return(m_strName);
        }

        public Integer getMnemonic()
        {
            return(m_intMnemonic);
        }
        
        public KeyStroke getAccelerator()
        {
        	return(m_ksAccelerator);
        }

        private String		m_strName;
        private Integer	m_intMnemonic;
        private KeyStroke	m_ksAccelerator;
    }

    public static String getString(String key)
    {
    	try
    	{
        	return(getBundle().getString(key));
    	}
    	catch(MissingResourceException e)
    	{
    		JOptionPane.showMessageDialog(null, "Missing "+key);
    		throw e;
    	}
    }

    public static String getString(String key, String theDefault)
    {
        try
        {
            return(getBundle().getString(key));
        }
        catch(MissingResourceException e)
        {
        }

        return(theDefault);
    }
    
    public static String getMessage(String key, String[] substitution)
    {
    	String template = getString(key);
    	
    	return(MessageFormat.format(template, substitution));
    }

    public static MenuInfo getMenuInfo(String key)
    {
        return(new MenuInfo(getString(key)));
    }

    public static ResourceBundle getBundle()
    {
        if (m_bundle == null)
        {
            if (m_locale == null)
            {
                m_bundle = ResourceBundle.getBundle("com.neom.wisp.wisper.gui.Text");
            }
            else
            {
                m_bundle = ResourceBundle.getBundle("com.neom.wisp.wisper.gui.Text", m_locale);
            }
        }

        return(m_bundle);
    }

    public static void setLocale(Locale theLocale)
    {
        m_locale = theLocale;
        m_bundle = null;
    }

    public static void setActionMenu(Action target, String strKey)
    {
        Text.MenuInfo info = Text.getMenuInfo(strKey);

        target.putValue(Action.NAME, info.getName());
        if (info.getMnemonic() != null)
        {
            target.putValue(Action.MNEMONIC_KEY, info.getMnemonic());
        }
        if (info.getAccelerator() != null)
        {
            target.putValue(Action.ACCELERATOR_KEY, info.getAccelerator());
        }
    }

    private static ResourceBundle   m_bundle;
    private static Locale           m_locale;
}