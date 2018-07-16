package com.neom.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/**
 * This class contains a variety of utility methods to make processing XML
 * documents easier.
 * 
 * @author Kevin Hunter
 * 
 * Copyright (c) 2003, NeoMedia Technologies, Inc.
 * All rights reserved.
 * 
 */

public class XmlUtil
{
	/**
	 * Creates a new DOM document with the specified root element name.
	 * 
	 * @param	strRootNodeName	The name of the root (document) element.
	 * @throws	<code>ParserConfigurationError</code>
	 */
	
	public static Document createDocument(String strRootNodeName) throws ParserConfigurationException
	{
    	if (m_docBuilder == null)
    	{
    		m_docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    	}
    	
    	Document doc = m_docBuilder.newDocument();
    	Element root = doc.createElement(strRootNodeName);
    	doc.appendChild(root);
    	
    	return(doc);
	}
	
	/**
	 * Loads a DOM document from an <code>InputStream</code>.
	 * 
	 * @param	is	<code>InputStream</code> from which to parse.
	 * @throws	<code>IOException</code>
	 * @throws	<code>SAXException</code>
	 */
	
	public static Document parseDocument(InputStream is)
			throws IOException, SAXException,  ParserConfigurationException
	{
    	if (m_docBuilder == null)
    	{
    		m_docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    	}
    	
		return(m_docBuilder.parse(is));
	}
	
	/**
	 * Loads a DOM document from an <code>InputStream</code> without throwing
	 * any exceptions.
	 * 
	 * @param	is	<code>InputStream</code> from which to parse.
	 * @throws	<code>IOException</code>
	 * @throws	<code>SAXException</code>
	 * @return DOM <code>Document</code> or <code>null</code> if an error occurs.
	 */
	
	public static Document parseDocumentSilent(InputStream is)
	{
		try
		{
			return(parseDocument(is));
		}
		catch(IOException e)
		{
		}
		catch(SAXException e)
		{
		}
		catch(ParserConfigurationException e)
		{
		}
		
		return(null);
	}
	
	public static Document parseDocumentSilent(File inputFile)
	{
		InputStream is = null;
		
		try
		{
			is = new BufferedInputStream(new FileInputStream(inputFile));
			return(parseDocument(is));
		}
		catch(IOException e)
		{
		}
		catch(SAXException e)
		{
		}
		catch(ParserConfigurationException e)
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
				catch(IOException e)
				{
				}
			}
		}
		
		return(null);
	}
	
	public static Document parseDocumentSilent(String strFileName)
	{
		return(parseDocumentSilent(new File(strFileName)));
	}
	
	public static void serializeDocument(Document doc, OutputStream os) throws TransformerConfigurationException, TransformerException
	{
    	if (m_docSerializer == null)
    	{
    		m_docSerializer = TransformerFactory.newInstance().newTransformer();
			m_docSerializer.setOutputProperty(OutputKeys.METHOD, "xml");
			m_docSerializer.setOutputProperty(OutputKeys.INDENT, "yes");
    	}
		
		m_docSerializer.transform(new DOMSource(doc), new StreamResult(os));
	}
	
	public static boolean serializeDocumentSilent(Document doc, OutputStream os)
	{
		try
		{
			serializeDocument(doc, os);
		}
		catch(TransformerException e)
		{
			return(false);
		}
		
		return(true);
	}
	
	public static boolean serializeDocumentSilent(Document doc, File outputFile)
	{
		OutputStream os = null;
		
		try
		{
        	os = new BufferedOutputStream(new FileOutputStream(outputFile));
			serializeDocument(doc, os);
		}
		catch(TransformerException e)
		{
			return(false);
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
				catch(IOException e)
				{
				}
			}
		}
		
		return(true);
	}
	
	public static void serializeDocumentSilent(Document doc, String strFileName)
	{
		serializeDocumentSilent(doc, new File(strFileName));
	}
	
    public static Element findChildElement(Element elemParent, String strChildName)
    {
    	Node theNode = elemParent.getFirstChild();
    	while(theNode != null)
    	{
    		if (theNode.getNodeType() == Node.ELEMENT_NODE &&
    			strChildName.equals(theNode.getNodeName()))
    		{
    			return((Element)theNode);
    		}
    		
    		theNode = theNode.getNextSibling();
    	}
    	
    	return(null);
    }
    
    public static Element findSiblingElement(Element elemStart, String strChildName)
    {
    	Node theNode = elemStart.getNextSibling();
    	while(theNode != null)
    	{
    		if (theNode.getNodeType() == Node.ELEMENT_NODE &&
    			strChildName.equals(theNode.getNodeName()))
    		{
    			return((Element)theNode);
    		}
    		
    		theNode = theNode.getNextSibling();
    	}
    	
    	return(null);
    }
    
    public static String getChildText(Element e, String defaultValue)
    {
    	e.normalize();
    	
    	Node textNode = e.getFirstChild();
    	if (textNode == null)
    	{
    		return(defaultValue);
    	}
    	
    	if (textNode.getNodeType() != Node.TEXT_NODE)
    	{
    		return(defaultValue);
    	}
    	
    	return(textNode.getNodeValue());
    }
    
    public static String getChildText(Element parent, String strChildName, String defaultValue)
    {
    	Element child = findChildElement(parent, strChildName);
    	if (child == null)
    	{
    		return(defaultValue);
    	}
    	
    	return(getChildText(child, defaultValue));
    }
    
    public static int getChildInteger(Element parent, String strChildName, int defaultValue)
    {
    	String strChildText = getChildText(parent, strChildName, null);
    	if (strChildText == null)
    	{
    		return(defaultValue);
    	}
    	
    	try
    	{
    		return(Integer.parseInt(strChildText));
    	}
    	catch(NumberFormatException e)
    	{
    	}
    	
    	return(defaultValue);
    }
    
    public static boolean getChildBoolean(Element parent, String strChildName, boolean bDefaultValue)
    {
    	String strChildText = getChildText(parent, strChildName, null);
    	if (strChildText == null)
    	{
    		return(bDefaultValue);
    	}
    	
    	if (strChildText.equals("true"))
    	{
    		return(true);
    	}
    	
    	if (strChildText.equals("false"))
    	{
    		return(false);
    	}
    	
    	return(bDefaultValue);
    }
    
    public static Element addChildElement(Document doc, Element parent, String strElemName)
    {
    	Element childElement = doc.createElement(strElemName);
    	
    	parent.appendChild(childElement);
    	
    	return(childElement);
    }
    
    public static Element addChildElement(Document doc, Element parent, String strElemName, String strValue)
    {
    	if (strValue == null)
    	{
    		strValue = "";
    	}
    	
    	Element childElement = doc.createElement(strElemName);
    	Text textNode = doc.createTextNode(strValue);
    	
    	parent.appendChild(childElement);
    	childElement.appendChild(textNode);
    	
    	return(childElement);
    }
    
    public static Element addChildElement(Document doc, Element parent, String strElemName, int value)
    {
    	return(addChildElement(doc, parent, strElemName, Integer.toString(value)));
    }

    public static Element addChildElement(Document doc, Element parent, String strElemName, boolean value)
    {
    	return(addChildElement(doc, parent, strElemName, value?"true":"false"));
    }

    private static DocumentBuilder	m_docBuilder;
    private static Transformer		m_docSerializer;
    
}
