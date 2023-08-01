package com.neom.wisp.wisper;

import javax.swing.ImageIcon;
import javax.swing.Icon;
import java.awt.Image;
import java.net.URL;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class Images
{
    public static final int WISPerIconLarge = 0;
    public static final int WISPerIconSmall = 1;
    public static final int RedLight = 2;
    public static final int GreenLight = 3;
    public static final int YellowLight = 4;
    public static final int NoLight = 5;
    public static final int BlackCheckOff = 6;
    public static final int BlackCheckOn = 7;
    public static final int WhiteCheckOff = 8;
    public static final int WhiteCheckOn = 9;

    private static class LoadInfo
    {
        public LoadInfo(int nIndex, String strPath)
        {
            m_nIndex = nIndex;
            m_strPath = strPath;
        }

        public int      m_nIndex;
        public String   m_strPath;
    }

    private static LoadInfo[] m_loadInfo =
    {
        new LoadInfo(WISPerIconLarge,   "/images/WISPerIconLarge.gif"),
        new LoadInfo(WISPerIconSmall,   "/images/WISPerIconSmall.gif"),
        new LoadInfo(RedLight,          "/images/RedLight.gif"),
        new LoadInfo(GreenLight,        "/images/GreenLight.gif"),
        new LoadInfo(YellowLight,       "/images/YellowLight.gif"),
        new LoadInfo(NoLight,           "/images/NoLight.gif"),
        new LoadInfo(BlackCheckOn,      "/images/BlackCheckOn.gif"),
        new LoadInfo(BlackCheckOff,     "/images/BlackCheckOff.gif"),
        new LoadInfo(WhiteCheckOn,      "/images/WhiteCheckOn.gif"),
        new LoadInfo(WhiteCheckOff,     "/images/WhiteCheckOff.gif"),
    };

    private static ImageIcon[] m_imageIcon;

    static
    {
        m_imageIcon = new ImageIcon[m_loadInfo.length];

        for (int i = 0; i < m_loadInfo.length; i++)
        {
            URL theUrl = Images.class.getResource(m_loadInfo[i].m_strPath);
            m_imageIcon[m_loadInfo[i].m_nIndex] = new ImageIcon(theUrl);
        }
    }

    public static Icon getIcon(int nIndex)
    {
        return(m_imageIcon[nIndex]);
    }

    public static Image getImage(int nIndex)
    {
        return(m_imageIcon[nIndex].getImage());
    }
}