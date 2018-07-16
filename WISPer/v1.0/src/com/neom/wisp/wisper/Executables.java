package com.neom.wisp.wisper;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      NeoMedia Technologies
 * @author Kevin Hunter
 * @version 1.0
 */

public class Executables
{
    public Executables()
    {
    }

    public String getTranslator()
    {
        return(m_strTranslator);
    }

    public void setTranslator(String value)
    {
        m_strTranslator = value;
    }

    public String getCompiler()
    {
        return(m_strCompiler);
    }

    public void setCompiler(String value)
    {
        m_strCompiler = value;
    }

    private String  m_strTranslator;
    private String  m_strCompiler;
}