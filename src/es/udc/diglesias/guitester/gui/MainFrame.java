/*
 * MainFrame.java
 *
 */
package es.udc.diglesias.guitester.gui;

import com.erlang4j.api.Erlang4jFactory;
import com.erlang4j.api.IErlang4j;
import com.erlang4j.api.IGetData;
import com.erlang4j.api.adapters.SwingMessageAdapter;
import com.erlang4j.api.process.IProcess;
import com.erlang4j.api.process.IProcessWithState;
import es.udc.diglesias.guitester.instrumentador.Editor;
import es.udc.diglesias.guitester.util.NutPad;
import es.udc.diglesias.guitester.instrumentador.TraceGetter;
import es.udc.diglesias.guitester.util.FilesystemOperations;
import java.io.File;
import java.io.IOException;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

public class MainFrame extends javax.swing.JFrame {

    private final IProcess process;

    /** Creates new form MainFrame */
    public MainFrame(IErlang4j erlang4j) {
        process = erlang4j.spawn(
                new SwingMessageAdapter("{Pid, {ok, generate_fsm}}") {

                    public void process(IProcessWithState process, IGetData data) throws Exception {
                        jLabelEstadoGeneracionFSM.setVisible(true);
                        jButtonVerFSM.setEnabled(true);
                    }
                },
                new SwingMessageAdapter("{Pid, {ok, generate_eqc}}") {

                    public void process(IProcessWithState process, IGetData data) throws Exception {
                        jLabelEstadoGeneracionQC.setVisible(true);
                    }
                },
                new SwingMessageAdapter("Any") {

                    public void process(IProcessWithState process, IGetData data) throws Exception {
                        System.out.println("Ignored message from Erlang server: "+data.getTuple("Any"));
                    }
                });
        initComponents();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jFileChooserBytecodes = new javax.swing.JFileChooser();
        jOptionPaneError = new javax.swing.JOptionPane();
        buttonGroupTipoCaso = new javax.swing.ButtonGroup();
        jFileChooserGuardar = new javax.swing.JFileChooser();
        jDialogVarEntorno = new javax.swing.JDialog();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextPane1 = new javax.swing.JTextPane();
        jTabbedPaneInstrumentar = new javax.swing.JTabbedPane();
        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        jButtonEditBytecodes = new javax.swing.JButton();
        jLabelEstadoEdicion = new javax.swing.JLabel();
        jPanelObtenerTrazas = new javax.swing.JPanel();
        jRadioButtonPositivo = new javax.swing.JRadioButton();
        jRadioButtonNegativo = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jButtonEjecutar = new javax.swing.JButton();
        jTextFieldClaseMain = new javax.swing.JTextField();
        jButtonStop = new javax.swing.JButton();
        jButtonVerTrazas = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        jButtonGenerarFSM = new javax.swing.JButton();
        jButtonVerFSM = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jLabelEstadoGeneracionFSM = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jButtonGenerarQC = new javax.swing.JButton();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jTextFieldModulo = new javax.swing.JTextField();
        jLabel11 = new javax.swing.JLabel();
        jLabelEstadoGeneracionQC = new javax.swing.JLabel();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItemGuardar = new javax.swing.JMenuItem();
        jMenuItemSalir = new javax.swing.JMenuItem();
        jMenu3 = new javax.swing.JMenu();
        jMenuItem1 = new javax.swing.JMenuItem();

        jFileChooserBytecodes.setFileSelectionMode(javax.swing.JFileChooser.DIRECTORIES_ONLY);

        jFileChooserGuardar.setDialogType(javax.swing.JFileChooser.SAVE_DIALOG);
        jFileChooserGuardar.setFileSelectionMode(javax.swing.JFileChooser.DIRECTORIES_ONLY);

        jScrollPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextPane1.setEditable(false);
        jTextPane1.setText("La aplicación utiliza por defecto ciertas aplicaciones auxiliares. Si se desean modificar, se han de establecer las siguientes variables de entorno:\n\n - GRAPHVIZ: nombre de la aplicación auxiliar que convierte un fichero en formato dot de Graphviz a una imagen JPEG. Será utilizada de la siguiente forma: $GRAPHVIZ -Tjpeg archivo.dot -oarchivo.jpeg. Por defecto es \"dot\".\n\n - VIEWER: nombre de la aplicación auxiliar que visualiza una imagen JPEG. Será utilizada de la siguiente forma: $VIEWER imagen.jpeg. Por defecto es \"open\".\n"); // NOI18N
        jScrollPane1.setViewportView(jTextPane1);

        javax.swing.GroupLayout jDialogVarEntornoLayout = new javax.swing.GroupLayout(jDialogVarEntorno.getContentPane());
        jDialogVarEntorno.getContentPane().setLayout(jDialogVarEntornoLayout);
        jDialogVarEntornoLayout.setHorizontalGroup(
            jDialogVarEntornoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jDialogVarEntornoLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 496, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jDialogVarEntornoLayout.setVerticalGroup(
            jDialogVarEntornoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jDialogVarEntornoLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 231, Short.MAX_VALUE)
                .addContainerGap())
        );

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("Herramienta de verificación de interfaces gráficas de usuario");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                formWindowClosing(evt);
            }
        });

        jLabel1.setText("Seleccione el paquete raíz de la aplicación. Se sobreescribirán los archivos, no se conservarán los originales.");

        jTextField1.setEditable(false);

        jLabel2.setText("Paquete raíz");

        jButton1.setText("Examinar...");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jButtonEditBytecodes.setText("Editar clases");
        jButtonEditBytecodes.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonEditBytecodesActionPerformed(evt);
            }
        });

        jLabelEstadoEdicion.setText("Completado");
        jLabelEstadoEdicion.setEnabled(false);
        jLabelEstadoEdicion.setVisible(false);

        buttonGroupTipoCaso.add(jRadioButtonPositivo);
        jRadioButtonPositivo.setSelected(true);
        jRadioButtonPositivo.setText("Caso positivo");

        buttonGroupTipoCaso.add(jRadioButtonNegativo);
        jRadioButtonNegativo.setText("Caso negativo (pulsar \"Stop\" al llegar al error)");
        jRadioButtonNegativo.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jRadioButtonNegativoItemStateChanged(evt);
            }
        });

        jLabel4.setText("¿Se tratará de un caso positivo (correcto) o negativo (error)?");

        jLabel5.setText("Clase principal:");

        jButtonEjecutar.setText("Ejecutar");
        jButtonEjecutar.setEnabled(false);
        jButtonEjecutar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonEjecutarActionPerformed(evt);
            }
        });

        jTextFieldClaseMain.setText("es.udc.ejemplo.MainClass");

        jButtonStop.setText("Stop");
        jButtonStop.setEnabled(false);
        jButtonStop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonStopActionPerformed(evt);
            }
        });

        jButtonVerTrazas.setText("Ver trazas almacenadas");
        jButtonVerTrazas.setEnabled(false);
        jButtonVerTrazas.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonVerTrazasActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanelObtenerTrazasLayout = new javax.swing.GroupLayout(jPanelObtenerTrazas);
        jPanelObtenerTrazas.setLayout(jPanelObtenerTrazasLayout);
        jPanelObtenerTrazasLayout.setHorizontalGroup(
            jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelObtenerTrazasLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelObtenerTrazasLayout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldClaseMain, javax.swing.GroupLayout.PREFERRED_SIZE, 269, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanelObtenerTrazasLayout.createSequentialGroup()
                        .addComponent(jRadioButtonPositivo)
                        .addGap(12, 12, 12)
                        .addComponent(jRadioButtonNegativo))
                    .addGroup(jPanelObtenerTrazasLayout.createSequentialGroup()
                        .addComponent(jButtonEjecutar)
                        .addGap(18, 18, 18)
                        .addComponent(jButtonStop))
                    .addComponent(jButtonVerTrazas)
                    .addComponent(jLabel4))
                .addContainerGap(213, Short.MAX_VALUE))
        );
        jPanelObtenerTrazasLayout.setVerticalGroup(
            jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelObtenerTrazasLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldClaseMain, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonNegativo)
                    .addComponent(jRadioButtonPositivo))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelObtenerTrazasLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonEjecutar)
                    .addComponent(jButtonStop))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonVerTrazas)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jLabel3.setText("Ejecute varias veces la aplicación para obtener trazas de ejecución de diferentes casos de uso.");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel1)
                    .addComponent(jPanelObtenerTrazas, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jButtonEditBytecodes)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelEstadoEdicion))
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jLabel2)
                                .addGap(18, 18, 18)
                                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, 236, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButton1))))
                    .addComponent(jLabel3))
                .addContainerGap(73, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButton1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonEditBytecodes)
                    .addComponent(jLabelEstadoEdicion))
                .addGap(24, 24, 24)
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelObtenerTrazas, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(60, Short.MAX_VALUE))
        );

        jTabbedPaneInstrumentar.addTab("Obtención de trazas", jPanel1);

        jButtonGenerarFSM.setText("Generar modelo");
        jButtonGenerarFSM.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonGenerarFSMActionPerformed(evt);
            }
        });

        jButtonVerFSM.setText("Ver modelo");
        jButtonVerFSM.setEnabled(false);
        jButtonVerFSM.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonVerFSMActionPerformed(evt);
            }
        });

        jLabel6.setText("Pulse el siguiente botón para generar la máquina de estados finitos correspondiente a la interfaz");

        jLabelEstadoGeneracionFSM.setText("Completado");
        jLabelEstadoGeneracionFSM.setEnabled(false);
        jLabelEstadoGeneracionFSM.setVisible(false);

        jLabel8.setText("Pulse el siguiente botón para mostrar gráficamente la máquina de estados finitos");

        jLabel7.setText("Las variables de entorno GRAPHVIZ y VIEWER han de estar establecidas (ver menú \"Ayuda\")");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel6)
                    .addComponent(jLabel8)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonVerFSM)
                            .addComponent(jLabel7)))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jButtonGenerarFSM)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelEstadoGeneracionFSM)))
                .addContainerGap(148, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel6)
                .addGap(12, 12, 12)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonGenerarFSM)
                    .addComponent(jLabelEstadoGeneracionFSM))
                .addGap(12, 12, 12)
                .addComponent(jLabel8)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel7)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonVerFSM)
                .addContainerGap(233, Short.MAX_VALUE))
        );

        jTabbedPaneInstrumentar.addTab("Generación de modelo", jPanel2);

        jButtonGenerarQC.setText("Generar módulo QuickCheck");
        jButtonGenerarQC.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonGenerarQCActionPerformed(evt);
            }
        });

        jLabel9.setText("Introduzca el nombre del módulo Erlang que desea generar");

        jLabel10.setText("Nombre del módulo");

        jTextFieldModulo.setText("quickcheck_skeleton");

        jLabel11.setText(".erl");

        jLabelEstadoGeneracionQC.setText("Completado");
        jLabelEstadoGeneracionQC.setEnabled(false);
        jLabelEstadoGeneracionQC.setVisible(false);

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel9)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addComponent(jButtonGenerarQC)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelEstadoGeneracionQC))
                            .addGroup(jPanel3Layout.createSequentialGroup()
                                .addComponent(jLabel10)
                                .addGap(18, 18, 18)
                                .addComponent(jTextFieldModulo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel11)))))
                .addContainerGap(386, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel9)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel10)
                    .addComponent(jTextFieldModulo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel11))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonGenerarQC)
                    .addComponent(jLabelEstadoGeneracionQC))
                .addContainerGap(293, Short.MAX_VALUE))
        );

        jTabbedPaneInstrumentar.addTab("Automatización de pruebas", jPanel3);

        jMenu1.setText("Archivo");

        jMenuItemGuardar.setText("Guardar datos...");
        jMenuItemGuardar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemGuardarActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItemGuardar);

        jMenuItemSalir.setText("Salir");
        jMenuItemSalir.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemSalirActionPerformed(evt);
            }
        });
        jMenu1.add(jMenuItemSalir);

        jMenuBar1.add(jMenu1);

        jMenu3.setText("Ayuda");

        jMenuItem1.setText("Variables de entorno");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });
        jMenu3.add(jMenuItem1);

        jMenuBar1.add(jMenu3);

        setJMenuBar(jMenuBar1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jTabbedPaneInstrumentar, javax.swing.GroupLayout.DEFAULT_SIZE, 790, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jTabbedPaneInstrumentar, javax.swing.GroupLayout.DEFAULT_SIZE, 439, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        int returnVal = jFileChooserBytecodes.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = jFileChooserBytecodes.getSelectedFile();
            // What to do with the file, e.g. display it in a TextArea
            if (!file.canRead() || !file.isDirectory()) {
                jOptionPaneError.setMessageType(javax.swing.JOptionPane.ERROR_MESSAGE);
                try {
                    jOptionPaneError.setMessage("No existe el directorio " + file.getCanonicalPath() + " o no se puede acceder");
                } catch (IOException ex) {
                    jOptionPaneError.setMessage("No existe el directorio o no se puede acceder");
                }
                JOptionPane.showMessageDialog(this, jOptionPaneError.getMessage(), null, jOptionPaneError.getMessageType(), jOptionPaneError.getIcon());
                jButton1ActionPerformed(null);
            } else {
                jTextField1.setText(file.getAbsolutePath());
                rootBytecodes = file;
            }
        }
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jButtonEditBytecodesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonEditBytecodesActionPerformed
        jLabelEstadoEdicion.setText("Editando...");
        jLabelEstadoEdicion.setVisible(true);
        Editor.main(rootBytecodes.getAbsolutePath() + System.getProperty("file.separator"),
                tmpDir.getAbsolutePath() + System.getProperty("file.separator"));
        jLabelEstadoEdicion.setText("Completado");
        jButtonEjecutar.setEnabled(true);
    }//GEN-LAST:event_jButtonEditBytecodesActionPerformed

    private void jButtonEjecutarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonEjecutarActionPerformed
        TraceGetter.addTrace(tmpDir, jTextFieldClaseMain.getText(), positive, tracesFile);
        jButtonVerTrazas.setEnabled(true);
    }//GEN-LAST:event_jButtonEjecutarActionPerformed

    private void jRadioButtonNegativoItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jRadioButtonNegativoItemStateChanged
        if (evt.getStateChange() == java.awt.event.ItemEvent.SELECTED) {
            jButtonStop.setEnabled(true);
            positive = false;
        } else {
            jButtonStop.setEnabled(false);
            positive = true;
        }
    }//GEN-LAST:event_jRadioButtonNegativoItemStateChanged

    private void jButtonStopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonStopActionPerformed
        TraceGetter.endTrace();
    }//GEN-LAST:event_jButtonStopActionPerformed

    private void jButtonVerTrazasActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonVerTrazasActionPerformed
        NutPad.open(tracesFile);
    }//GEN-LAST:event_jButtonVerTrazasActionPerformed

    private void formWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosing
        deleteTempFolder();
    }//GEN-LAST:event_formWindowClosing

    private void jMenuItemSalirActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemSalirActionPerformed
        deleteTempFolder();
        System.exit(0);
    }//GEN-LAST:event_jMenuItemSalirActionPerformed

    private void jMenuItemGuardarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemGuardarActionPerformed
        int returnVal = jFileChooserGuardar.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = jFileChooserGuardar.getSelectedFile();
            // What to do with the file, e.g. display it in a TextArea
            if (!file.canRead() || !file.isDirectory()) {
                jOptionPaneError.setMessageType(javax.swing.JOptionPane.ERROR_MESSAGE);
                try {
                    jOptionPaneError.setMessage("No existe el directorio " + file.getCanonicalPath() + " o no se puede acceder");
                } catch (IOException ex) {
                    jOptionPaneError.setMessage("No existe el directorio o no se puede acceder");
                }
                JOptionPane.showMessageDialog(this, jOptionPaneError.getMessage(), null, jOptionPaneError.getMessageType(), jOptionPaneError.getIcon());
            } else {
                try {
                    FilesystemOperations.copyDirectory(tmpDir, file);
                } catch (IOException ex) {
                    System.out.println("No se puede copiar al directorio");
                }
            }
        }
    }//GEN-LAST:event_jMenuItemGuardarActionPerformed

    private void jButtonGenerarFSMActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonGenerarFSMActionPerformed
        process.send("swing_listener", "{Self,{generate_fsm, P0}}", tracesFile.getAbsolutePath());
    }//GEN-LAST:event_jButtonGenerarFSMActionPerformed

    private void jButtonVerFSMActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonVerFSMActionPerformed
        process.send("swing_listener", "{Self,{view_fsm, P0}}", tmpDir.getAbsolutePath());
    }//GEN-LAST:event_jButtonVerFSMActionPerformed

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        jDialogVarEntorno.pack();
        jDialogVarEntorno.setVisible(true);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void jButtonGenerarQCActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonGenerarQCActionPerformed
        process.send("swing_listener", "{Self,{generate_eqc, P0, P1}}", jTextFieldModulo.getText(), tmpDir.getAbsolutePath());
    }//GEN-LAST:event_jButtonGenerarQCActionPerformed

    public static void main(String args[]) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
            System.out.println("Cannot set system Look&Feel");
        }

        //Erlang4j
        final IErlang4j erlang4j = Erlang4jFactory.make("erlangVM", "galleta");


        java.awt.EventQueue.invokeLater(new Runnable() {

            public void run() {
                new MainFrame(erlang4j).setVisible(true);
            }
        });
    }
    //My variables
    private File rootBytecodes;
    private File tmpDir = findTmpDir();
    private File tracesFile = findTracesFile();
    private boolean positive = true;
    //End of my variables
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupTipoCaso;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButtonEditBytecodes;
    private javax.swing.JButton jButtonEjecutar;
    private javax.swing.JButton jButtonGenerarFSM;
    private javax.swing.JButton jButtonGenerarQC;
    private javax.swing.JButton jButtonStop;
    private javax.swing.JButton jButtonVerFSM;
    private javax.swing.JButton jButtonVerTrazas;
    private javax.swing.JDialog jDialogVarEntorno;
    private javax.swing.JFileChooser jFileChooserBytecodes;
    private javax.swing.JFileChooser jFileChooserGuardar;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLabel jLabelEstadoEdicion;
    private javax.swing.JLabel jLabelEstadoGeneracionFSM;
    private javax.swing.JLabel jLabelEstadoGeneracionQC;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItemGuardar;
    private javax.swing.JMenuItem jMenuItemSalir;
    private javax.swing.JOptionPane jOptionPaneError;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanelObtenerTrazas;
    private javax.swing.JRadioButton jRadioButtonNegativo;
    private javax.swing.JRadioButton jRadioButtonPositivo;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTabbedPane jTabbedPaneInstrumentar;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextFieldClaseMain;
    private javax.swing.JTextField jTextFieldModulo;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    private File findTmpDir() {
        String tempDir = System.getProperty("java.io.tmpdir");
        if (!(tempDir.endsWith("/") || tempDir.endsWith("\\"))) {
            tempDir = tempDir + System.getProperty("file.separator");
        }
        tempDir = tempDir + "guiTesterData" + System.getProperty("file.separator");
        System.out.println("Temporary directory is " + tempDir);
        return new File(tempDir);
    }

    private File findTracesFile() {
        return new File(tmpDir, "trazas.txt");
    }

    private void deleteTempFolder() {
        System.out.println("Deleting temporary directory...");
        FilesystemOperations.deleteDirectory(tmpDir);
    }
}
