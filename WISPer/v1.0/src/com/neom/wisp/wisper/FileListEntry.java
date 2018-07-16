package com.neom.wisp.wisper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

/*
 *  "External" files
 *  FILE.WCB    Contains Wang Cobol Source
 *  FILE.cob    Contains translated Cobol Source (output of translator)
 *  FILE.obj    Contains compiled object file (output of Cobol compiler)
 *
 *  "Internal" files
 *  FILE.werr   Contains error text from translation process
 *  FILE.cerr   Contains error text from Cobol compile
 *  FILE.wspr   Contains "light" codes from translate and compile
 *                  wisp.status=N   Translator result
 *                  cobol.status=N  Compiler result
 */

public class FileListEntry implements Comparable
{
    public static final int NO_LIGHT = 0;
    public static final int GREEN_LIGHT = 1;
    public static final int YELLOW_LIGHT = 2;
    public static final int RED_LIGHT = 3;

    public FileListEntry(File f)
    {
        m_fWcbFile = f;
        m_fDirectory = f.getParentFile();

        String strFileName = f.getName();
        m_strBaseName = strFileName.substring(0,strFileName.length()-4);

        m_fCobolFile = new File(m_fDirectory, m_strBaseName + ".cob");
        m_fWispError = new File(m_fDirectory, m_strBaseName + ".werr");
        m_fCobolError = new File(m_fDirectory, m_strBaseName + ".cerr");
        m_fStatus = new File(m_fDirectory, m_strBaseName + ".wspr");

        if (m_fStatus.exists())
        {
            loadStatus();
        }
        else
        {
            m_nWispLight = NO_LIGHT;
            m_nCobolLight = NO_LIGHT;
        }
    }

    public String toString()
    {
        return(getWcbFileName());
    }

    public int compareTo(Object o)
    {
        if (Main.isSystemCaseSensitive())
        {
            return(getWcbFileName().compareTo(o.toString()));
        }

        return(getWcbFileName().toLowerCase().compareTo(o.toString().toLowerCase()));
    }

    public String getWcbFileName()
    {
        return(m_fWcbFile.getName());
    }

    public String getWispErrorFileName()
    {
        return(m_fWispError.getName());
    }

    public String getCobolFileName()
    {
        return(m_fCobolFile.getName());
    }

    public String getCobolErrorFileName()
    {
        return(m_fCobolError.getName());
    }

    public String getBaseName()
    {
        return(m_strBaseName);
    }

    public File getWcbFile()
    {
        return(m_fWcbFile);
    }

    public File getCobolFile()
    {
        return(m_fCobolFile);
    }

    public File getDirectory()
    {
        return(m_fDirectory);
    }

    public File getWispErrorFile()
    {
        return(m_fWispError);
    }

    public File getCobolErrorFile()
    {
        return(m_fCobolError);
    }

    public File getStatusFile()
    {
        return(m_fStatus);
    }

    public void setWispLight(int nValue)
    {
        m_nWispLight = nValue;
    }

    public int getWispLight()
    {
        return(m_nWispLight);
    }

    public void setCobolLight(int nValue)
    {
        m_nCobolLight = nValue;
    }

    public int getCobolLight()
    {
        return(m_nCobolLight);
    }

    public void setWispCommand(String cmd)
    {
        m_strWispCommand = cmd;
    }

    public void setCobolCommand(String cmd)
    {
        m_strCobolCommand = cmd;
    }

    public boolean isChecked()
    {
        return(m_bCheck);
    }

    public void setCheck(boolean bCheck)
    {
        m_bCheck = bCheck;
    }

    public void invertCheck()
    {
        m_bCheck = !m_bCheck;
    }

    public void loadStatus()
    {
        FileInputStream is = null;

        m_nWispLight = NO_LIGHT;
        m_nCobolLight = NO_LIGHT;

        try
        {
            is = new FileInputStream(m_fStatus);
            Properties p = new Properties();
            p.load(is);
            m_nWispLight = Integer.parseInt(p.getProperty("wisp.status", "0"));
            m_nCobolLight = Integer.parseInt(p.getProperty("cobol.status", "0"));
        }
        catch(IOException e)
        {
        }
        catch(NumberFormatException e)
        {
        }
        finally
        {
            if (is != null)
            {
                try
                {
                    is.close();
                }
                catch(Exception e)
                {
                }
            }
        }
    }

    public boolean saveStatus()
    {
        FileOutputStream os = null;

        try
        {
            os = new FileOutputStream(m_fStatus);
            Properties p = new Properties();
            p.setProperty("wisp.status", Integer.toString(m_nWispLight));
            p.setProperty("cobol.status", Integer.toString(m_nCobolLight));
            if (m_strWispCommand != null)
            {
                p.setProperty("wisp.command", m_strWispCommand);
            }
            if (m_strCobolCommand != null)
            {
                p.setProperty("wisp.command", m_strCobolCommand);
            }
            p.store(os, "WISPer status for "+getWcbFileName());
        }
        catch(IOException e)
        {
            return(false);
        }
        finally
        {
            if (os != null)
            {
                try
                {
                    os.close();
                }
                catch(Exception e)
                {
                }
            }
        }

        return(true);
    }

    public void cleanFiles()
    {
        if (m_fWispError.exists())
        {
            m_fWispError.delete();
        }

        if (m_fCobolError.exists())
        {
            m_fCobolError.delete();
        }

        if (m_fStatus.exists())
        {
            m_fStatus.delete();
        }
    }

    private File        m_fWcbFile;
    private File        m_fCobolFile;
    private File        m_fDirectory;
    private File        m_fWispError;
    private File        m_fCobolError;
    private File        m_fStatus;

    private String      m_strBaseName;

    private int         m_nWispLight = NO_LIGHT;
    private int         m_nCobolLight = NO_LIGHT;
    private String      m_strWispCommand;
    private String      m_strCobolCommand;
    private boolean     m_bCheck = false;
}

