package com.neom.wisp.wisper.gui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.util.Locale;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.UIManager;

import com.neom.wisp.wisper.UserSettings;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class Main
{
    public static void main(String[] args)
    {
        String osName = System.getProperty("os.name").toLowerCase();
        if (osName.indexOf("windows") >= 0)
        {
            m_bWindows = true;
        }
        else
        {
            m_bWindows = false;
        }
        
        loadFonts();

        Text.setLocale(Locale.getDefault());
        UserSettings.load();
        
        try
        {
        	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        }
        catch(Exception e)
        {
        }

        m_mainWindow = new MainWindow();
        m_mainWindow.show();
    }
    
    public static MainWindow getMainWindow()
    {
    	return(m_mainWindow);
    }
    
    public static JMenu buildMenu(String strKey, Action[] contents)
    {
        Text.MenuInfo info = Text.getMenuInfo(strKey);

        JMenu menu = new JMenu(info.getName());
        if (info.getMnemonic() != null)
        {
            menu.setMnemonic(info.getMnemonic().intValue());
        }

        for (int i = 0; i < contents.length; i++)
        {
            if (contents[i] == null)
            {
                menu.addSeparator();
            }
            else
            {
                menu.add(new JMenuItem(contents[i]));
            }
        }

        return(menu);
    }

    public static void setMinSize(Window w)
    {
        String strMinX = Text.getString(w.getName()+".minX", null);
        if (strMinX == null)
        {
            return;
        }
        String strMinY = Text.getString(w.getName()+".minY", null);
        if (strMinY == null)
        {
            return;
        }

        int minX, minY;
        try
        {
            minX = Integer.parseInt(strMinX);
            minY = Integer.parseInt(strMinY);
        }
        catch(NumberFormatException e)
        {
            return;
        }

        Dimension size = w.getSize();
        if (size.width >= minX && size.height >= minY)
        {
            return;
        }

        if (size.width < minX)
        {
            size.width = minX;
        }

        if (size.height < minY)
        {
            size.height = minY;
        }

        w.setSize(size);
    }

    public static void setMaxSize(Window w)
    {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = w.getSize();
        if (windowSize.width > screenSize.width * 4 / 5)
        {
        	windowSize.width = screenSize.width * 4 / 5;
        }
        if (windowSize.height > screenSize.height * 4 / 5)
        {
        	windowSize.height = screenSize.height * 4 / 5;
        }
        w.setSize(windowSize);
    }
    
    public static void centerInScreen(Window w)
    {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = w.getSize();
        Point location = new Point( (screenSize.width - windowSize.width)/2,
                                    (screenSize.height - windowSize.height)/2);
        w.setLocation(location);
    }

    public static boolean isWindows()
    {
        return(m_bWindows);
    }

    public static boolean isUnix()
    {
        return(!m_bWindows);
    }

    public static boolean isSystemCaseSensitive()
    {
        return(isUnix());
    }
    
    public static Font getEditorFont()
    {
    	return(m_fontEditor);
    }
    
    public static String getEditorFontFamily()
    {
    	return(m_strFontFamily);
    }

    public static Font getTabTitleFont()
    {
    	return(m_fontTabTitle);
    }
    
    public static int getEditorFontSize()
    {
    	return(12);
    }
    
    private static void loadFonts()
    {
    	m_fontTabTitle = new Font("Monospaced", Font.PLAIN, getEditorFontSize());
    	
    	String[] familyNames = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
    	m_strFontFamily = "Monospaced";
    	
		String[] preferredFontFamilies =
		{
			"Courier New",
			"Courier"
		};
		
		outer:
		for (int i = 0; i < preferredFontFamilies.length; i++)
		{
			for (int j = 0; j < familyNames.length; j++)
			{
				if (familyNames[j].equals(preferredFontFamilies[i]))
				{
					m_strFontFamily = preferredFontFamilies[i];
					break outer;
				}
			}
		}
		
		m_fontEditor = new Font(m_strFontFamily, Font.PLAIN, getEditorFontSize());
    }
    
    private static boolean	m_bWindows;
    private static String		m_strFontFamily;
    private static Font		m_fontEditor;
    private static Font		m_fontTabTitle;
    private static MainWindow	m_mainWindow;
}