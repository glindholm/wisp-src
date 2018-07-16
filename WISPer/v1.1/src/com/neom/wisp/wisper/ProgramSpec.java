package com.neom.wisp.wisper;

import java.io.*;

/**
 * @author khunter
 * This class contains the data required to invoke one of the subprograms
 * (compiler or translator).  It contains the path to the executable, as well
 * as a template that specifies the arguments that should be used when invoking
 * the executable.  The template may contain the following "substitution points":
 * 
 * 		[b]	The base name (without the extension) of the source file being
 * 			processed.
 * 		[B]	Same as [b], except that the name will be converted to all upper case.
 * 		[c]	The full path name of the translated file.  This is the same as the
 * 			original file name, except that the .wcb extension is converted to .cob
 * 		[d]	The full path name to the directory containing the original source file.
 * 		[f]	The full file name (with extension, but without directory) of the
 * 			original source file.
 * 		[p]	The full path name of the original source file.
 * 
 * 
 */

public class ProgramSpec implements Cloneable
{
	public ProgramSpec()
	{
	}
	
	public ProgramSpec(String strExecutable, String strArgumentTemplate)
	{
		m_strExecutable = strExecutable;
		m_strArgumentTemplate = strArgumentTemplate;
	}
	
	public ProgramSpec(ProgramSpec other)
	{
		m_strExecutable = other.m_strExecutable;
		m_strArgumentTemplate = other.m_strArgumentTemplate;
	}
	
    public Object clone()
    {
    	Object theClone = null;
    	
        try
        {
            theClone = super.clone();
        }
        catch(CloneNotSupportedException e)
        {
            e.printStackTrace(System.err);
            System.exit(1);
        }

        return(theClone);
    }

	public void setExecutable(String strExecutable)
	{
		m_strExecutable = strExecutable;
	}
	
	public String getExecutable()
	{
		return(m_strExecutable);
	}
	
	public void setArgumentTemplate(String strArgumentTemplate)
	{
		m_strArgumentTemplate = strArgumentTemplate;
	}
	
	public String getArgumentTemplate()
	{
		return(m_strArgumentTemplate);
	}
	
	public boolean isValid()
	{
		if (m_strArgumentTemplate == null ||
			m_strExecutable == null)
		{
			return(false);
		}
		
		if (validateArgumentTemplate(m_strArgumentTemplate) != null)
		{
			return(false);
		}
		
		return(validateExecutable(m_strExecutable));
	}
	
	public boolean isTemplateValid()
	{
		if (validateArgumentTemplate(m_strArgumentTemplate) != null)
		{
			return(false);
		}
		
		return(true);
	}
	
	public String buildCommand(SourceModule item)
	{
		return(buildCommand(item, null));
	}
	
    public String buildCommand(SourceModule item, String strPrefix)
    {
        if (m_strExecutable == null)
        {
            return(null);
        }

        if (m_strArgumentTemplate == null)
        {
            return(null);
        }

        StringBuffer buffer = new StringBuffer();
        if (strPrefix != null)
        {
        	buffer.append(strPrefix);
        	buffer.append(' ');
        }
        
        if (m_strExecutable.indexOf(' ') >= 0)
        {
        	buffer.append('"');
        	buffer.append(m_strExecutable);
        	buffer.append('"');
        }
        else
        {
        	buffer.append(m_strExecutable);
        }
        
        buffer.append(' ');
        
        int i = 0;
        int nLength = m_strArgumentTemplate.length();
        while(i < nLength)
        {
            char c = m_strArgumentTemplate.charAt(i);
            if (c != '[')
            {
                buffer.append(c);
                i++;
                continue;
            }
            
            if (i >= nLength - 1)
            {
                return(null);
            }
            
            i++;
            
            c = m_strArgumentTemplate.charAt(i);
            if (c == '[')
            {
            	buffer.append('[');
            	i++;
            	continue;
            }

            if (i > nLength - 2)
            {
                return(null);
            }
            
            if (m_strArgumentTemplate.charAt(i+1) != ']')
            {
            	return(null);
            }

            switch(c)
            {
            case 'b':
                buffer.append(item.getFileBase());
                break;
            case 'B':
                buffer.append(item.getFileBase().toUpperCase());
                break;
            case 'c':
                buffer.append(item.getTranslatedFile().getAbsolutePath());
                break;
            case 'd':
                buffer.append(item.getSourceDirectory().getAbsolutePath());
                break;
            case 'f':
                buffer.append(item.getSourceFile().getName());
                break;
            case 'p':
                buffer.append(item.getSourceFile().getAbsolutePath());
                break;
            default:
                return(null);
            }
            
            i += 2;
        }

        return(buffer.toString());
    }

	private String		m_strExecutable;
	private String		m_strArgumentTemplate;
	
	public static boolean validateExecutable(String strExecutableFile)
	{
		File theFile = new File(strExecutableFile);
		if (!theFile.exists())
		{
			return(false);
		}
		
		if (!theFile.isFile())
		{
			return(false);
		}
		
		return(true);
	}
	
	/**
	 * This function validates a candidate argument template.  It returns
	 * <code>null</code> if the argument template is valid.  Otherwise, it
	 * returns an instance of <code>InvalidRange</code> which describes the
	 * portion of the template to which it objects.
	 */
	
	public static InvalidRange validateArgumentTemplate(String strTemplate)
	{
        int i = 0;
        int nLength = strTemplate.length();

        while(i < nLength)
        {
            char c = strTemplate.charAt(i);
            if (c != '[')
            {
            	i++;
            	continue;
            }
            
            if (i >= nLength - 1)
            {
                // opening brace at end of template.
                return(new InvalidRange(i, nLength-1));
            }

            if (strTemplate.charAt(i+1) == '[')
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

            if (strTemplate.charAt(i+2) != ']')
            {
            	// "[<char><char other than ]>"
                return(new InvalidRange(i, i+2));
            }

            if ("bBcdfp".indexOf(strTemplate.charAt(i+1)) < 0)
            {
            	// "[<char other than valid substitution point>]"
                return(new InvalidRange(i, i+2));
            }

			i += 3;
        }

        return(null);
	}
	
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

}
