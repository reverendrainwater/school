import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class fmGUITest extends JFrame
	{
		private int row = 0;
		private int col = 0;
		
		private static final long serialVersionUID = 1L;
    
		private static final int FRAME_SIZE1 = 300;
		private static final int FRAME_SIZE2 = 400;
		private static final int GRID_ROW = 3;
		private static final int GRID_COL = 3;
		private static final int GRID_PADH = 5; // Grid padding horizontal.
		private static final int GRID_PADV = 10; // Grid padding vertical.
		
		private JFrame frame;
		
		private JPanel arrayPanel;
		private JPanel buttonsPanel;
		
		private JLabel labelRow;
		private JLabel labelCol;
		private JLabel labelString;
		private JLabel labelOutput;
		
		private JTextField row;
		private JTextField col;
		private JTextField string;
		
		private JButton createTable;
		private JButton processString;
		private JButton reset;
		private JButton submitToDAP;
		
	/**
     * Constructor.
     */
    public fmGUITest()
    {
        initUI();
		
		buttonsPanel.add(labelRow);
		buttonsPanel.add(numRow);
		buttonsPanel.add(labelCol);
		buttonsPanel.add(numCol);
        buttonsPanel.add(createTable);
        buttonsPanel.add(processString);
        buttonsPanel.add(reset);
        buttonsPanel.add(submitToDAP);
        
        frame.add(arrayPanel, BorderLayout.WEST);
		
        frame.setVisible(true);
        arrayPanel.setVisible(true);
		buttonsPanel.setVisible(true);
        
		frame.add(buttonsPanel, BorderLayout.EAST);
    }
    
    /**
     * Initialize User Interface.
     * 
     */
    public void initUI()
    {
        frame = new JFrame();
        frame.setSize(FRAME_SIZE1, FRAME_SIZE2);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setTitle("Finite State Machine Transition Tables"); 
        
        arrayPanel = new JPanel();
        buttonsPanel = new JPanel();
		
		buttonsPanel.setLayout(new GridLayout(GRID_ROW, GRID_COL, GRID_PADH, GRID_PADV);

        labelRow = new JLabel("Row: ");
        labelRow.setName("numRowLab");
		labelCol = new JLabel("Col: ");
		labelCol.setName("numColLab");
		labelString = new JLabel("String: ");
		labelCol.setName("stringLab");
		labelOutput = new JLabel("
        
        numRow = new JTextField();
        numRow.setColumns(3);
        numRow.setName("numRow");
		numCol = new JTextField();
        numCol.setColumns(3);
        numCol.setName("numCol");
        
        createTable = new JButton("Create Table");
        createTable.setName("createTable");
        processString = new JButton("Process String");
        processString.setName("processString");
        reset = new JButton("Reset");
        reset.setName("reset");
        submit = new JButton("Submit");
        submit.setName("submitProg");
        
        createTable.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                createTableArray();
            }
        });
        processString.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                procString();
            }
        });
        reset.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                resetArray();
            }
        });
        submit.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                submitProg();
            }
        });
    }
}