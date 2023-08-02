package com.neom.wisp.wisper;

import java.util.Properties;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.File;
import java.awt.Toolkit;
import java.awt.Dimension;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class UserSettings
{
    private static final String MainWnd_Width = "MainWnd.Width";
    private static final String MainWnd_Height = "MainWnd.Height";
    private static final String MainWnd_X = "MainWnd.X";
    private static final String MainWnd_Y = "MainWnd.Y";
    private static final String CurrentDirectory = "CurrentDirectory";
    private static final String CurrentDirectorySel = "CurrentDirectorySel";
    private static final String VerticalSplitterPos = "VerticalSplitterPos";
    private static final String HorizontalSplitterPos = "HorizontalSplitterPos";
    private static final String Translator = "Translator";
    private static final String Compiler = "Compiler";
    private static final String TranslatorArgs = "TranslatorArgs";
    private static final String CompilerArgs = "CompilerArgs";

    private static final String SettingsDir = ".WISPer";
    private static final String SettingsFile = "WISPer.settings";

    private static Properties   m_properties = new Properties();
    private static String       m_strSettingsDirPath;
    private static String       m_strSettingsFilePath;

    static
    {
        StringBuffer buffer = new StringBuffer();
        buffer.append(System.getProperty("user.home"));
        buffer.append(File.separator);
        buffer.append(SettingsDir);
        m_strSettingsDirPath = buffer.toString();

        buffer.append(File.separator);
        buffer.append(SettingsFile);
        m_strSettingsFilePath = buffer.toString();
    }

    public static void load()
    {
        FileInputStream is = null;

        try
        {
            is = new FileInputStream(m_strSettingsFilePath);
            m_properties.load(is);
        }
        catch(IOException e)
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

            setDefaults();
        }
    }

    public static void save()
    {
        File theDir = new File(m_strSettingsDirPath);
        if (!theDir.exists())
        {
            if (!theDir.mkdir())
            {
                return;
            }
        }

        FileOutputStream os = null;
        try
        {
            os = new FileOutputStream(m_strSettingsFilePath);
            m_properties.store(os, "WISPer settings");
        }
        catch(IOException e)
        {
            setDefaults();
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
    }

    public static String getSetting(String key)
    {
        return(m_properties.getProperty(key));
    }

    public static void setSetting(String key, String value)
    {
        if (value == null)
        {
            value = "";
        }

        m_properties.setProperty(key, value);
    }

    public static int getIntSetting(String key)
    {
        String strValue = m_properties.getProperty(key);
        try
        {
            return(Integer.parseInt(strValue));
        }
        catch(NumberFormatException e)
        {
        }

        return(-1);
    }

    public static void setIntSetting(String key, int value)
    {
        m_properties.setProperty(key, Integer.toString(value));
    }

    private static void setDefaults()
    {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        if (getIntSetting(MainWnd_Width) < 0)
        {
            setIntSetting(MainWnd_Width, screenSize.width * 8 / 10);
        }

        if (getIntSetting(MainWnd_Height) < 0)
        {
            setIntSetting(MainWnd_Height, screenSize.height * 8 / 10);
        }

        if (getIntSetting(MainWnd_X) < 0)
        {
            setIntSetting(MainWnd_X, screenSize.width / 10);
        }

        if (getIntSetting(MainWnd_Y) < 0)
        {
            setIntSetting(MainWnd_Y, screenSize.height / 10);
        }

        if (getSetting(CurrentDirectory) == null)
        {
            setSetting(CurrentDirectory, System.getProperty("user.home"));
        }
    }

    public static int getMainWndWidth()
    {
        return(getIntSetting(MainWnd_Width));
    }
    public static void setMainWndWidth(int n)
    {
        setIntSetting(MainWnd_Width, n);
    }

    public static int getMainWndHeight()
    {
        return(getIntSetting(MainWnd_Height));
    }
    public static void setMainWndHeight(int n)
    {
        setIntSetting(MainWnd_Height, n);
    }

    public static int getMainWndX()
    {
        return(getIntSetting(MainWnd_X));
    }
    public static void setMainWndX(int n)
    {
        setIntSetting(MainWnd_X, n);
    }

    public static int getMainWndY()
    {
        return(getIntSetting(MainWnd_Y));
    }
    public static void setMainWndY(int n)
    {
        setIntSetting(MainWnd_Y, n);
    }

    public static String getCurrentDirectory()
    {
        return(getSetting(CurrentDirectory));
    }
    public static void setCurrentDirectory(String value)
    {
        setSetting(CurrentDirectory, value);
    }

    public static String getCurrentDirectorySel()
    {
        return(getSetting(CurrentDirectorySel));
    }
    public static void setCurrentDirectorySel(String value)
    {
        setSetting(CurrentDirectorySel, value);
    }

    public static int getVerticalSplitterPos()
    {
        return(getIntSetting(VerticalSplitterPos));
    }
    public static void setVerticalSplitterPos(int n)
    {
        setIntSetting(VerticalSplitterPos, n);
    }

    public static int getHorizontalSplitterPos()
    {
        return(getIntSetting(HorizontalSplitterPos));
    }
    public static void setHorizontalSplitterPos(int n)
    {
        setIntSetting(HorizontalSplitterPos, n);
    }

    public static Executables getExecutables()
    {
        Executables exes = new Executables();
        exes.setTranslator(getSetting(Translator));
        exes.setCompiler(getSetting(Compiler));
        return(exes);
    }
    public static void setExecutables(Executables exes)
    {
        setSetting(Translator, exes.getTranslator());
        setSetting(Compiler, exes.getCompiler());
    }

    public static Arguments getArguments()
    {
        Arguments args = new Arguments();
        args.setTranslatorArgs(getSetting(TranslatorArgs));
        args.setCompilerArgs(getSetting(CompilerArgs));
        return(args);
    }
    public static void setArguments(Arguments args)
    {
        setSetting(TranslatorArgs, args.getTranslatorArgs());
        setSetting(CompilerArgs, args.getCompilerArgs());
    }

    public static void loadDirectoryArguments(File dir, String file, Arguments args)
    {
        File settingsFile = new File(dir, file);
        FileInputStream is = null;
        try
        {
            is = new FileInputStream(settingsFile);
            Properties p = new Properties();
            p.load(is);
            args.setTranslatorArgs(p.getProperty(TranslatorArgs));
            args.setCompilerArgs(p.getProperty(CompilerArgs));
            if (args.getTranslatorArgs() != null && args.getCompilerArgs() != null)
            {
                args.setUseGlobal(false);
            }
            else
            {
                args.setUseGlobal(true);
            }
        }
        catch(IOException e)
        {
            args.setUseGlobal(true);
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

    public static boolean saveDirectoryArguments(File dir, String file, Arguments args)
    {
        File settingsFile = new File(dir, file);
        if (args.getUseGlobal())
        {
            if (settingsFile.exists())
            {
                return(settingsFile.delete());
            }

            return(true);
        }
        FileOutputStream os = null;
        try
        {
            Properties p = new Properties();
            p.setProperty(TranslatorArgs, args.getTranslatorArgs());
            p.setProperty(CompilerArgs, args.getCompilerArgs());
            os = new FileOutputStream(settingsFile);
            p.store(os, "WISPer settings");
        }
        catch(Exception e)
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
}