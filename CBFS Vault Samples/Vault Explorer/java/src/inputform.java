import javax.swing.*;
import java.awt.event.*;

public class inputform extends JDialog {
    private JPanel panelMain;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JTextField textfieldValue;
    private JLabel labelPrompt;

    private String strTitle;
    private String strPrompt;

    public  String strValue;

    public inputform(){
        initForm("", "", "");
    }

    public inputform(String title){
        initForm(title, "", "");
    }

    public  inputform(String title, String prompt){
        initForm(title, prompt, "");
    }

    public inputform(String title, String prompt, String value) {
        initForm(title, prompt, value);
    }

    private void initForm(String title, String prompt, String value) {

        strTitle = title;
        strPrompt = prompt;
        strValue = value;

        setContentPane(panelMain);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        buttonCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        });

        // call onCancel() when cross is clicked
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowOpened(WindowEvent e) {
                onInit();
            }
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        // call onCancel() on ESCAPE
        panelMain.registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    private void onInit() {

        this.setTitle(strTitle);
        labelPrompt.setText(strPrompt);
        textfieldValue.setText(strValue);

        pack();
    }

    private void onOK() {
        strValue = textfieldValue.getText();
        dispose();
    }

    private void onCancel() {
        strValue = "";
        dispose();
    }
}
