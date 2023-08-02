package com.neom.wisp.wisper;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public class Arguments implements Cloneable
{
    public static final int OK  = 0;

    public Arguments()
    {
    }

    public boolean getUseGlobal()
    {
        return(m_bUseGlobal);
    }

    public void setUseGlobal(boolean b)
    {
        m_bUseGlobal = b;
    }

    public ArgumentTemplate getTranslatorTemplate()
    {
        return(m_argTranslator);
    }

    public void setTranslatorTemplate(ArgumentTemplate t)
    {
        m_argTranslator.setTemplate(t.getTemplate());
    }

    public String getTranslatorArgs()
    {
        return(m_argTranslator.getTemplate());
    }

    public void setTranslatorArgs(String value)
    {
        m_argTranslator.setTemplate(value);
    }

    public ArgumentTemplate getCompilerTemplate()
    {
        return(m_argCompiler);
    }

    public void setCompilerTemplate(ArgumentTemplate t)
    {
        m_argCompiler.setTemplate(t.getTemplate());
    }

    public String getCompilerArgs()
    {
        return(m_argCompiler.getTemplate());
    }

    public void setCompilerArgs(String value)
    {
        m_argCompiler.setTemplate(value);
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
        return(null);
    }

    public Arguments copy()
    {
        return((Arguments)this.clone());
    }

    private boolean             m_bUseGlobal = true;
    private ArgumentTemplate    m_argTranslator = new ArgumentTemplate();
    private ArgumentTemplate    m_argCompiler = new ArgumentTemplate();
}