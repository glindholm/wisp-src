package com.neom.wisp.wisper;
import java.util.HashMap;
import java.io.File;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class ArgumentTemplate implements Cloneable
{
    public static class InvalidRange
    {
        private InvalidRange(int nStart, int nEnd)
        {
            m_nStart = nStart;
            m_nEnd = nEnd;
        }

        public int getStart()
        {
            return(m_nStart);
        }

        public int getEnd()
        {
            return(m_nEnd);
        }

        private int m_nStart;
        private int m_nEnd;
    }

    /*
     *  Returns null on success - otherwise returns fragment it doesn't like
     */

    public static InvalidRange findInvalidPortion(String s)
    {
        int i = 0;
        int nLength = s.length();

        while(i < nLength)
        {
            char c = s.charAt(i);
            if (c == '[')
            {
                if (i >= nLength - 1)
                {
                    // trailing open brace
                    return(new InvalidRange(i, nLength-1));
                }

                if (s.charAt(i+1) == '[')
                {
                    //"[[" -> '[', OK
                    i += 2;
                    continue;
                }

                if (i >= nLength - 2)
                {
                    // "[<char><end of string>"
                    return(new InvalidRange(i, nLength-1));
                }

                if (s.charAt(i+2) != ']')
                {
                    return(new InvalidRange(i, i+2));
                }

                if ("bBcdfp".indexOf(s.charAt(i+1)) < 0)
                {
                    return(new InvalidRange(i, i+2));
                }

                i += 3;
            }
            else
            {
                i++;
            }
        }

        return(null);
    }

    public ArgumentTemplate()
    {
        this("");
    }

    public ArgumentTemplate(String s)
    {
        m_strTemplate = s;
    }

    public void setTemplate(String s)
    {
        m_strTemplate = s;
    }

    public String getTemplate()
    {
        return(m_strTemplate);
    }

    public Object clone()
    {
        try
        {
            return(super.clone());
        }
        catch(CloneNotSupportedException e)
        {
            e.printStackTrace(System.err);
        }

        return(new ArgumentTemplate(m_strTemplate));
    }

    public void copy(ArgumentTemplate other)
    {
        m_strTemplate = other.m_strTemplate;
    }

    public String substitute(FileListEntry item)
    {
        if (m_strTemplate == null)
        {
            return(null);
        }

        StringBuffer buffer = new StringBuffer();
        int i = 0;
        int nLength = m_strTemplate.length();
        while(i < nLength)
        {
            char c = m_strTemplate.charAt(i);
            if (c != '[')
            {
                buffer.append(c);
                i++;
            }
            else
            {
                if (i >= nLength - 1)
                {
                    return(null);
                }

                switch(m_strTemplate.charAt(i+1))
                {
                case '[':
                    buffer.append('[');
                    i += 2;
                    break;
                case 'b':
                    buffer.append(item.getBaseName());
                    i += 3;
                    break;
                case 'B':
                    buffer.append(item.getBaseName().toUpperCase());
                    i += 3;
                    break;
                case 'c':
                    buffer.append(item.getCobolFile().getAbsolutePath());
                    i += 3;
                    break;
                case 'd':
                    buffer.append(item.getDirectory().getAbsolutePath());
                    i += 3;
                    break;
                case 'f':
                    buffer.append(item.getWcbFile().getName());
                    i += 3;
                    break;
                case 'p':
                    buffer.append(item.getWcbFile().getAbsolutePath());
                    i += 3;
                    break;
                default:
                    return(null);
                }
            }
        }

        return(buffer.toString());
    }

    private String  m_strTemplate;
}