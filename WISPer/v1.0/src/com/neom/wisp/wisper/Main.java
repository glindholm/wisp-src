package com.neom.wisp.wisper;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.util.Locale;

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

        Text.setLocale(Locale.getDefault());
        UserSettings.load();

        new MainWnd().show();
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

    private static boolean m_bWindows;
}