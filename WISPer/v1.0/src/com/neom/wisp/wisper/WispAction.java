package com.neom.wisp.wisper;

import javax.swing.Action;
import javax.swing.AbstractAction;

/**
 * Title:        WISPer
 * Description:
 * Copyright:    Copyright (c) 2002
 * Company:      Shell Stream Software LLC
 * @author Kevin Hunter
 * @version 1.0
 */

public abstract class WispAction extends AbstractAction
{
    public WispAction(String strKey)
    {
        Text.MenuInfo info = Text.getMenuInfo(strKey);

        putValue(Action.NAME, info.getName());
        if (info.getMnemonic() != null)
        {
            putValue(Action.MNEMONIC_KEY, info.getMnemonic());
        }
    }
}