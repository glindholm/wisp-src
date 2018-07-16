package com.neom.wisp.wisper;
import com.neom.util.*;
import org.w3c.dom.*;
import java.io.*;
import java.util.*;

/**
 * @author khunter
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class ProcessResult
{
	public static final int NOT_RUN	= 0;
	public static final int FAILED	= 1;
	public static final int WARNING	= 2;
	public static final int SUCCESS	= 3;

	public ProcessResult()
	{
		m_nState = NOT_RUN;
	}
	
	public void setState(int nState)
	{
		m_nState = nState;
	}
	
	public int getState()
	{
		return(m_nState);
	}
	
	public void setCommandLine(String strCommandLine)
	{
		m_strCommandLine = strCommandLine;
	}
	
	public String getCommandLine()
	{
		return(m_strCommandLine);
	}
	
	public void abort(String strMessage)
	{
		if (getOutputByteCount() > 0)
		{
			try
			{
				String strEol = System.getProperty("line.separator");
				byte[] b = strEol.getBytes();
				m_bufResult.write(b);
				m_bufResult.write(strMessage.getBytes());
			}
			catch(IOException e)
			{
			}
		}
		else
		{
			m_outputLines = new String[1];
			m_outputLines[0] = strMessage;
		}
	}
	
	public void load(Element root)
	{
		m_nState = XmlUtil.getChildInteger(root, ElemState, NOT_RUN);
		m_strCommandLine = XmlUtil.getChildText(root, ElemCommand, "");
		m_bufResult = null;
		m_outputLines = null;
		
		Element outputRoot = XmlUtil.findChildElement(root, ElemOutput);
		if (outputRoot == null)
		{
			return;
		}
		
		ArrayList list = new ArrayList();
		Element line = XmlUtil.findChildElement(outputRoot, ElemLine);
		while(line != null)
		{
			list.add(XmlUtil.getChildText(line, ""));
			line = XmlUtil.findSiblingElement(line, ElemLine);
		}
		m_outputLines = (String[])list.toArray(new String[list.size()]);
	}
	
	public void save(Document doc, Element root)
	{
		XmlUtil.addChildElement(doc, root, ElemState, m_nState);
		XmlUtil.addChildElement(doc, root, ElemCommand, m_strCommandLine);
		if (m_outputLines != null)
		{
			Element outputRoot = XmlUtil.addChildElement(doc, root, ElemOutput);
			for (int i = 0; i < m_outputLines.length; i++)
			{
				XmlUtil.addChildElement(doc, outputRoot, ElemLine, m_outputLines[i]);
			}
		}
	}
	
	public void beginProcessOutput()
	{
		m_bufResult = new ByteArrayOutputStream();
		m_outputLines = null;
	}
	
	public int getOutputByteCount()
	{
		if (m_bufResult != null)
		{
			return(m_bufResult.size());
		}
		
		return(0);
	}
	
	public int getOutputLineCount()
	{
		if (m_outputLines != null)
		{
			return(m_outputLines.length);
		}
		
		return(0);
	}
	
	public String[] getOutputLines()
	{
		return(m_outputLines);
	}
	
	public synchronized void addToProcessOutput(byte[] buf, int nStart, int nCount)
	{
		m_bufResult.write(buf, nStart, nCount);
	}
	
	public void finishProcessOutput()
	{
		if (m_bufResult == null)
		{
			return;
		}
		
		String results = m_bufResult.toString();
		int len = results.length();
		m_bufResult = null;
		
		ArrayList list = new ArrayList();
		
		String strEol = System.getProperty("line.separator");
		int nEolLen = strEol.length();
		int nIndex = 0;
		
		while(nIndex < len)
		{
			int nEol = results.indexOf(strEol, nIndex);
			if (nIndex < 0)
			{
				list.add(results.substring(nIndex, len));
				break;
			}
			else
			{
				list.add(results.substring(nIndex, nEol));
				nIndex = nEol + nEolLen;
			}
		}
		
		m_outputLines = (String[])list.toArray(new String[list.size()]);
	}
	
	public void setStateTranslate(int nReturnCode)
	{
		if (nReturnCode != 0)
		{
			m_nState = FAILED;
			return;
		}
		
		m_nState = SUCCESS;
		for (int i = 0; i < m_outputLines.length; i++)
		{
			if (m_outputLines[i].length() > 0 &&
				m_outputLines[i].charAt(0) == '%')
			{
				m_nState = WARNING;
				break;
			}
		}
	}
	
	public void setStateCompile()
	{
		if (m_outputLines.length > 0)
		{
			m_nState = FAILED;
		}
		else
		{
			m_nState = SUCCESS;
		}
	}

	private String					m_strCommandLine;
	private int 					m_nState;
	private ByteArrayOutputStream	m_bufResult;
	private String[]				m_outputLines;
	
	private static final String ElemState	= "state";
	private static final String ElemCommand	= "command";
	private static final String ElemOutput	= "output";
	private static final String ElemLine		= "line";
}
