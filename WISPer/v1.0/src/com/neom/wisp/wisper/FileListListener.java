package com.neom.wisp.wisper;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public interface FileListListener
{
    public void selectionChanged(FileListEntry item);
    public void checksChanged(int nChecked);
}