package com.neom.wisp.wisper;

import java.util.ResourceBundle;
import java.util.Locale;
import java.util.MissingResourceException;

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
            }
            else
            {
                m_strName = strText.substring(0, nCommaPos);
                if (nCommaPos < strText.length() - 1)
                {
                    m_intMnemonic = new Integer((int)strText.charAt(nCommaPos+1));
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

        private String  m_strName;
        private Integer m_intMnemonic;
    }

    public static String getString(String key)
    {
        return(getBundle().getString(key));
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
                m_bundle = ResourceBundle.getBundle("com.neom.wisp.wisper.Text");
            }
            else
            {
                m_bundle = ResourceBundle.getBundle("com.neom.wisp.wisper.Text", m_locale);
            }
        }

        return(m_bundle);
    }

    public static void setLocale(Locale theLocale)
    {
        m_locale = theLocale;
        m_bundle = null;
    }

    private static ResourceBundle   m_bundle;
    private static Locale           m_locale;
}