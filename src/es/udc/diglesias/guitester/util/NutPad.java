package es.udc.diglesias.guitester.util;

// Text file viewer slightly based on Fred Swartz's NutPad
// Author: David Iglesias Fraga
// Original author : Fred Swartz - 2006-12-14 - Placed in public domain.
// Original purpose: Illustrates use of AbstractActions for menus.
//          It only uses a few Action features.  Many more are available.
//          This program uses the obscure "read" and "write"
//               text component methods.

import java.awt.*;
import javax.swing.*;
import java.io.*;

///////////////////////////////////////////////////////////////////////// NutPad
public class NutPad extends JFrame implements Runnable {
    
    private final File file;
    //... Components
    private JTextArea _editArea;

    public static void open(File file) {
        NutPad n = new NutPad(file);
        new Thread(n).start();
    }

    private NutPad(File file) {
        this.file = file;
    }

    public void run() {
        //... Create scrollable text area.
        _editArea = new JTextArea(15, 80);
        _editArea.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        _editArea.setFont(new Font("monospaced", Font.PLAIN, 14));
        JScrollPane scrollingText = new JScrollPane(_editArea);
        try {
            FileReader reader = new FileReader(file);
            _editArea.read(reader, "");  // Use TextComponent read
        } catch (IOException ioex) {
            System.out.println(ioex);
        }

        //-- Create a content pane, set layout, add component.
        JPanel content = new JPanel();
        content.setLayout(new BorderLayout());
        content.add(scrollingText, BorderLayout.CENTER);


        //... Set window content and menu.
        setContentPane(content);

        //... Set other window characteristics.
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setTitle("NutPad based viewer");
        pack();
        setLocationRelativeTo(null);
        setVisible(true);
    }
}
