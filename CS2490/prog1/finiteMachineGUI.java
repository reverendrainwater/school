// Finite State Machine with GUI
// Accepts strings over {a,b} that contain aabab, bbbab, or baaa.
// Rev. Taylor Rainwater, 9/8/15

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

public class finiteMachine extends JFrame
	{
		private int row = 0;
		private static final int COLUMNS = 3;
		
		private static final long serialVersionUID = 1L;
    
		private static final int FRAME_SIZE1 = 150;
		private static final int FRAME_SIZE2 = 400;
		
		private JFrame frame;
		
		private JPanel bottPanel;
		private JPanel midPanel;
		private JPanel topPanel;
		private JLabel label;
		
		private JTextField ;
		private JTextField textLast;
		
		private JButton arraySize;
		private JButton submitTable;
		private JButton reset;
		private JButton submitToDAP;
		
	/**
     * Constructor.
     */
    public Calculator()
    {
        initialComp();
                
        bottPanel.add(addButton);
        bottPanel.add(subButton);
        bottPanel.add(divButton);
        bottPanel.add(multiButton);
        
        frame.setVisible(true);
        bottPanel.setVisible(true);
        
        frame.add(bottPanel, BorderLayout.PAGE_END);
        
        midPanel.add(label);
        
        frame.add(midPanel, BorderLayout.LINE_START);
        
        topPanel.add(textFirst);
        topPanel.add(textLast);
        
        frame.add(topPanel, BorderLayout.PAGE_START);
    }
    
    /**
     * Initialize Components.
     * 
     */
    public void initialComp()
    {
        frame = new JFrame();
        frame.setLocation(100, 100);
        frame.setSize(FRAME_SIZE1, FRAME_SIZE2);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setTitle("Finite State Machine"); 
        
        bottPanel = new JPanel();
        midPanel = new JPanel();
        topPanel = new JPanel();
                
        label = new JLabel("Enter the number of rows: ");
        label.setName("numRows");
        
        textFirst = new JTextField();
        textFirst.setColumns(3);
        textFirst.setName("a");
        textMid = new JTextField();
        textMid.setColumns(3);
        textMid.setName("b");
		textLast = new JTextField();
        textLast.setColumns(3);
        textLast.setName("c");
        
        addButton = new JButton("Array Size");
        addButton.setName("arraySize");
        subButton = new JButton("Submit Table");
        subButton.setName("submitTable");
        divButton = new JButton("Reset");
        divButton.setName("reset");
        multiButton = new JButton("Submit to Teacher");
        multiButton.setName("submitToDAP");
        
        addButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                createArray();
            }
        });
        subButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                updateArrayWithTable();
            }
        });
        divButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
                reset();
            }
        });
        multiButton.addActionListener(new ActionListener()
        //Leeloo has multipass.
        {
            public void actionPerformed(ActionEvent e)
            {
                submitToStudent();
            }
        });
    }
		
		private int[][] STATE_TABLE = new int[row][COLUMNS];

		private BufferedReader in;


		public finiteMachine()
		{
			in = new BufferedReader(
					 new InputStreamReader(System.in));
		}


		public void run() throws IOException
		{
			char ch;
			int  state;

			for (;;) {
				System.out.print("Enter your string: ");
				ch    = (char) in.read();
				state = 0;

				while (ch != '\n') {
				   state = STATE_TABLE[state][charToColumn(ch)];
				   ch    = (char) in.read();
				}

				if (state == 10) {
					System.out.println("Accept\n");
				} else {
					System.out.println("Reject\n");
				}
			}
		}

		public int charToColumn(char ch)
		{
			int column = 2;

			switch( ch ) {
			case 'a':
				column = 0;
				break;

			case 'b':
				column = 1;
				break;
			}

			return column;
		}

		public static void main(String [] args) 
		{
			try {
				finiteMachine fsm = new finiteMachine();
				fsm.run();
			} catch (IOException ex) {
				ex.printStackTrace();
				System.exit(1);
			}
		}
}
