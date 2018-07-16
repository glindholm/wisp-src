package com.neom.wisp.wisper.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;

import com.neom.wisp.wisper.SourceModule;
import com.neom.wisp.wisper.SourceModuleList;
import com.neom.wisp.wisper.SourceModuleListener;

/**
 * @author khunter
 *
 */

public class MainWindowFilePane extends JList implements SourceModuleListener
{
	public MainWindowFilePane(MainWindow parent)
	{
        super(new FileListModel());
        
        m_model = (FileListModel)getModel();
		m_parent = parent;
		
        addMouseListener(new FileListMouseListener());
        setCellRenderer(new ListEntryRenderer());
	}
	
	public void load(SourceModuleList list)
	{
		m_model.empty(this);
		
		File[] files = list.getCurrentModuleFiles();
		FileListEntry[] entries = new FileListEntry[files.length];
		int n = 0;
		int i;
		for (i = 0; i < files.length; i++)
		{
			SourceModule sm = list.getModule(files[i]);
			if (sm != null)
			{
				sm.loadProperties();
				entries[n++] = new FileListEntry(sm);
			}
		}
		
		Arrays.sort(entries, 0, n);
		for (i = 0; i < n; i++)
		{
			m_model.addElement(entries[i]);
			entries[i].getSourceModule().addSourceModuleListener(this);
		}
		
		fireChecksChanged();
	}
	
	public void restoreChecks(String[] checkedFileNames)
	{
		int nEntries = getCount();
		for (int i = 0; i < nEntries; i++)
		{
			FileListEntry entry = getEntry(i);
			String strFileName = entry.getSourceModule().getSourceFile().getName();
			for (int j = 0; j < checkedFileNames.length; j++)
			{
				if (strFileName.equals(checkedFileNames[j]))
				{
					entry.setChecked(true);
					break;
				}
			}
		}
		
		fireChecksChanged();
	}
	
	public void moduleTranslated()
	{
		m_model.update();
	}
	
	public void moduleCompiled()
	{
		m_model.update();
	}
	
	public void moduleCleared()
	{
		m_model.update();
	}
	
    public FileListEntry getEntry(int n)
    {
        return((FileListEntry)m_model.elementAt(n));
    }
    
    public int getCount()
    {
    	return(m_model.size());
    }
    
    public int getCheckedCount()
    {
    	int n = 0;
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		if (getEntry(i).isChecked())
    		{
    			n++;
    		}
    	}
    	
    	return(n);
    }
    
    public SourceModule[] getCheckedModules()
    {
    	ArrayList list = new ArrayList();
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		FileListEntry entry = getEntry(i);
    		if (entry.isChecked())
    		{
    			list.add(entry.getSourceModule());
    		}
    	}
    	
    	return((SourceModule[])list.toArray(new SourceModule[list.size()]));
    }
    
    public String[] getCheckedFileNames()
    {
    	ArrayList list = new ArrayList();
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		FileListEntry entry = getEntry(i);
    		if (entry.isChecked())
    		{
    			list.add(entry.getSourceModule().getSourceFile().getName());
    		}
    	}
    	
    	return((String[])list.toArray(new String[list.size()]));
    }
    
    public void checkAll()
    {
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		getEntry(i).setChecked(true);
    	}
    	m_model.updateAll();
    	fireChecksChanged();
    }
    
    public void checkNone()
    {
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		getEntry(i).setChecked(false);
    	}
    	m_model.updateAll();
    	fireChecksChanged();
    }
    
    public void invertChecks()
    {
    	int count = getCount();
    	for (int i = 0; i < count; i++)
    	{
    		getEntry(i).invertCheck();
    	}
    	m_model.updateAll();
    	fireChecksChanged();
    }
    
    protected void fireChecksChanged()
    {
    	int nChecked = getCheckedCount();
    	m_parent.checksChanged(nChecked);
    }
    
    protected void launchWindow(int nIndex)
    {
    	SourceModule sm = getEntry(nIndex).getSourceModule();
		SourceWindow win = sm.getSourceWindow();
		if (win == null)
		{
			win = new SourceWindow(sm);
		}
		
		win.setState(Frame.NORMAL);
		win.toFront();
    }

	private MainWindow			m_parent;
	private FileListModel		m_model;
	private int				m_nCheckIconWidth;
	
    private static class FileListModel extends DefaultListModel
    {
        public FileListModel()
        {
        }

        public void updateAll()
        {
            int nCount = size();
            if (nCount > 0)
            {
                update(0, nCount-1);
            }
        }

		public void update()
		{
			update(0, size()-1);
		}
		
        public void update(int nStart, int nEnd)
        {
            fireContentsChanged(this, nStart, nEnd);
        }
        
        public void empty(SourceModuleListener listener)
        {
        	int nCount = getSize();
        	for (int i = 0; i < nCount; i++)
        	{
        		FileListEntry entry = (FileListEntry)get(i);
        		entry.getSourceModule().removeSourceModuleListener(listener);
        	}
        	
        	removeAllElements();
        }
    }

	private static class FileListEntry implements Comparable
	{
		public FileListEntry(SourceModule sm)
		{
			m_sourceModule = sm;
		}
		
	    public int compareTo(Object o)
	    {
	        if (Main.isSystemCaseSensitive())
	        {
	            return(m_sourceModule.getSourceFile().getName().compareTo(o.toString()));
	        }
	
            return(m_sourceModule.getSourceFile().getName().compareToIgnoreCase(o.toString()));
	    }
	    
	    public String toString()
	    {
	    	return(m_sourceModule.getSourceFile().getName());
	    }
	    
	    public SourceModule getSourceModule()
	    {
	    	return(m_sourceModule);
	    }
	    
	    public boolean isChecked()
	    {
	    	return(m_bIsChecked);
	    }
	    
	    public void setChecked(boolean bIsChecked)
	    {
	    	m_bIsChecked = bIsChecked;
	    }
	    
	    public void invertCheck()
	    {
	    	m_bIsChecked = !m_bIsChecked;
	    }

		private SourceModule	m_sourceModule;
		private boolean		m_bIsChecked;
	}
	
    private class ListEntryRenderer extends JPanel implements ListCellRenderer
    {
        public ListEntryRenderer()
        {
            FlowLayout layout = new FlowLayout(FlowLayout.LEFT);
            layout.setVgap(0);
            setLayout(layout);
            
            m_textPart.setFont(Main.getEditorFont());

            add(m_checkBox);
            add(m_translationLight);
            add(m_compileLight);
            add(m_textPart);

            m_iconList[0] = Images.getIcon(Images.NoLight);
            m_iconList[1] = Images.getIcon(Images.RedLight);
            m_iconList[2] = Images.getIcon(Images.YellowLight);
            m_iconList[3] = Images.getIcon(Images.GreenLight);

            m_checkBox.setIcon(Images.getIcon(Images.WhiteCheckOn));
            m_translationLight.setIcon(m_iconList[0]);
            m_compileLight.setIcon(m_iconList[0]);
            validate();

            MainWindowFilePane.this.m_nCheckIconWidth =
                    layout.getHgap() * 2 +
                    m_checkBox.getPreferredSize().width;

            m_foreSelected = UIManager.getColor("List.selectionForeground");
            m_backSelected = UIManager.getColor("List.selectionBackground");
            m_foreUnselected = UIManager.getColor("List.foreground");
            m_backUnselected = UIManager.getColor("List.background");
        }

        public Component getListCellRendererComponent(  JList list,
                                                        Object value,
                                                        int index,
                                                        boolean isSelected,
                                                        boolean cellHasFocus)
        {
            FileListEntry entry = MainWindowFilePane.this.getEntry(index);
            SourceModule sm = entry.getSourceModule();

            m_translationLight.setIcon(m_iconList[sm.getTranslationState()]);
            m_compileLight.setIcon(m_iconList[sm.getCompileState()]);
            m_textPart.setText(entry.toString());

            if (isSelected)
            {
                setBackground(m_backSelected);
                m_checkBox.setBackground(m_backSelected);
                m_textPart.setForeground(m_foreSelected);

                if (entry.isChecked())
                {
                    m_checkBox.setIcon(Images.getIcon(Images.WhiteCheckOn));
                }
                else
                {
                    m_checkBox.setIcon(Images.getIcon(Images.WhiteCheckOff));
                }
            }
            else
            {
                setBackground(m_backUnselected);
                m_checkBox.setBackground(m_backUnselected);
                m_textPart.setForeground(m_foreUnselected);

                if (entry.isChecked())
                {
                    m_checkBox.setIcon(Images.getIcon(Images.BlackCheckOn));
                }
                else
                {
                    m_checkBox.setIcon(Images.getIcon(Images.BlackCheckOff));
                }
            }

            return(this);
        }

        private JLabel  m_checkBox			= new JLabel();
        private JLabel  m_translationLight	= new JLabel();
        private JLabel  m_compileLight		= new JLabel();
        private JLabel  m_textPart			= new JLabel();
        private Icon[]  m_iconList			= new Icon[4];
        private Color   m_foreSelected;
        private Color   m_backSelected;
        private Color   m_foreUnselected;
        private Color   m_backUnselected;
    }
    
    private class FileListMouseListener extends MouseAdapter
    {
        public FileListMouseListener()
        {
        }

        public void mouseClicked(MouseEvent e)
        {
            Point p = e.getPoint();
            int nIndex = MainWindowFilePane.this.locationToIndex(p);
            if (nIndex >= 0 && nIndex < MainWindowFilePane.this.getCount())
            {
                if (p.x < MainWindowFilePane.this.m_nCheckIconWidth)
                {
                	MainWindowFilePane.this.getEntry(nIndex).invertCheck();
                	MainWindowFilePane.this.m_model.update(nIndex, nIndex);
                	MainWindowFilePane.this.fireChecksChanged();
                }
                else
                {
                	if (e.getClickCount() == 2)
                	{
                		MainWindowFilePane.this.launchWindow(nIndex);
                	}
                }
            }
        }
    }
}
