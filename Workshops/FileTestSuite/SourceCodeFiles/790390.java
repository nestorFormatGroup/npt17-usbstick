import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.reflect.*;

class cal1  /*extends Applet*/ implements ActionListener,WindowListener{
    
	int undocount=0;
	public int simulate=0;
	
	GridBagConstraints gbc;
	
	MenuItem FileNew=new MenuItem("New");
	MenuItem FileOpen=new MenuItem("Open");
	MenuItem FileSave=new MenuItem("Save");
	MenuItem FileExit=new MenuItem("Exit");
	MenuItem HelpAbout=new MenuItem("About");	
	MenuItem HelpContents=new MenuItem("Contents");	
	MenuItem undo=new MenuItem("Undo");	
	MenuItem redo=new MenuItem("Redo");	
	MenuItem change_scale=new MenuItem("Change Scale");	
	MenuItem simulation=new MenuItem("Lathe Simulation");	


	Frame mainframe=new Frame("Computer Aided Lathe - [Noname.cal]");
		
	Panelmain panelmain=new Panelmain();
	Panelclient  panelclient=new Panelclient(1,0,20,"",this,mainframe);
	Panelcommand panelcommand=new Panelcommand(panelclient);
	Panelstatus panelstatus=new Panelstatus(panelclient);

	


	MenuBar menubar=new MenuBar();
				
	Menu file=new Menu("File");
	Menu edit=new Menu("Edit");
	Menu tools=new Menu("Tools");
	Menu help=new Menu("Help");
	
	
	//CONSTRUCTOR
    
	public	cal1(){
		

		file.add(FileNew);
		file.addSeparator();
		file.add(FileOpen);
		file.addSeparator();
		file.add(FileSave);
		file.addSeparator();
		file.add(FileExit);

			
		edit.add(undo);
		edit.addSeparator();
		edit.add(redo);
		
		tools.add(change_scale);
		tools.addSeparator();
		tools.add(simulation);
	
		redo.disable();
		undo.disable();

		help.add(HelpContents);
		help.addSeparator();
		help.add(HelpAbout);
		
		menubar.add(file);
		menubar.add(edit);
		menubar.add(tools);
		menubar.add(help);

		mainframe.addWindowListener(this);
		
		FileNew.addActionListener(this);
		FileSave.addActionListener(this);
		FileOpen.addActionListener(this);
		FileExit.addActionListener(this);
		HelpAbout.addActionListener(this);
		HelpContents.addActionListener(this);
		change_scale.addActionListener(this);
		undo.addActionListener(this);
		redo.addActionListener(this);
		simulation.addActionListener(this);

		mainframe.setMenuBar(menubar);
		mainframe.resize(640,460);
		mainframe.setBackground(new Color(175,187,205));
		mainframe.setResizable(false);
		

		panelmain.setLayout(new GridBagLayout());
		panelmain.setBackground(new Color(0,142,142));
		
	
		panelstatus.setBackground(new Color(0,142,142));
		panelcommand.setBackground(new Color(175,172,158));
		
		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=0;
		gbc.weightx=2;
		gbc.weighty=6;
		gbc.fill=GridBagConstraints.BOTH;
		panelmain.add(panelclient,gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=0;
		gbc.weightx=1;
		gbc.weighty=6;
		gbc.fill=GridBagConstraints.BOTH;
		panelmain.add(panelstatus,gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=1;
		gbc.weightx=2;
		gbc.weighty=1;
		gbc.fill=GridBagConstraints.BOTH;
		panelmain.add(panelcommand,gbc);

		mainframe.add(panelmain);
		mainframe.show();

	}
    
	public  static void main(String agrs[]){//void init(){
		new cal1();
	}


	public void windowOpened(WindowEvent e){
	}
	
	public void windowClosing(WindowEvent e){
		if(e.getWindow().equals(mainframe))
			System.exit(0);
	}
	
	public void windowClosed(WindowEvent e){
	}
	
	public void windowIconified(WindowEvent e){
	}
	
	public void windowActivated(WindowEvent e){
	}
	
	public void windowDeiconified(WindowEvent e){
	}
	
	public void windowDeactivated(WindowEvent e){
	}
	
	public void actionPerformed(ActionEvent e){
		String STR=e.getActionCommand();
		if(STR.equals("New")){
			Tempdialog td=new Tempdialog(panelclient,mainframe,panelstatus,this);
		}
		
		if(STR.equals("About")){
			new OnHelpAbout(mainframe);
		}

		if(STR.equals("Contents")){
			new OnHelpContents(mainframe);
		}

		
		if(STR.equals("Undo")){
			if(panelclient.counter!=0){
				panelclient.noOfLines--;
				panelclient.counter--;
				panelclient.paintStatus=0;
				panelclient.repaint();
				undocount++;
				
			
				if (panelclient.counter<panelclient.maxcounter)
					redo.enable();
				else redo.disable();
				
				redo.enable();
				
				if (panelclient.counter==0)
					undo.disable();
			 	
				if (panelcommand.front=="Horizontal ")
					panelcommand.front="Vertical ";
				else panelcommand.front="Horizontal ";
				
				panelcommand.text_command.setText(panelcommand.front);
				
				if (panelclient.flip==0)
					panelclient.flip=1;
				else panelclient.flip=0;
				panelstatus.repaint();
			 
			}
			else undo.disable();
		 
		}
		
		if(STR.equals("Redo")){	
			if (panelclient.maxcounter>=panelclient.counter){
				panelclient.noOfLines++;
				panelclient.counter++;
				panelclient.paintStatus=0;
				panelclient.repaint();

				undocount--;
				undo.enable();
				if (panelclient.maxcounter==panelclient.counter)
					redo.disable();
				
				if (panelcommand.front=="Horizontal ") 
					panelcommand.front="Vertical ";
				else panelcommand.front="Horizontal ";
				
				panelcommand.text_command.setText(panelcommand.front);
				
				if (panelclient.flip==0)
					panelclient.flip=1;
				else panelclient.flip=0;
			
				panelstatus.repaint();
			
			}
			else redo.disable();

		}
		 
		if(STR.equals("Save")){	
			panelclient.save(mainframe); 
		
		}

		if(STR.equals("Lathe Simulation")){	
			/*try{
				RandomAccessFile Myfile=new RandomAccessFile("temprary.cal","rw");
				Myfile.writeShort(-10);
				Myfile.writeShort(panelclient.length);
				Myfile.writeShort(panelclient.counter);
			
				for(int i=1;i<=panelclient.counter; i++){
					Myfile.writeShort(panelclient.global_int[i]);
					System.out.println("global int "+i+"= "+panelclient.global_int[i]);
				}
				System.exit(0);	
			}catch(Exception ee){ 
			} 					 */


			simulate=1;
			panelclient.paintStatus=0;		 
			panelclient.startSimulation();
					 
		
		}
		
		if(STR.equals("Open")){	
			panelclient.open(mainframe); 
		
		}
		
		if(STR.equals("Exit")){
			System.exit(0);
		}
		
		if(STR.equals("Change Scale")){
			new changeScale(this);
		}
		 
	}
}



class changeScale extends Frame implements ActionListener{
	
	cal1 cal;
	Dialog dialogsave; 
	
    
	//Buttons
	
	Button cancel=new Button("Cancel");				
	Button ok=new Button("OK");

	//Label

	Label label1=new Label("Scale ");
		
	TextField text_scale=new TextField(10);
	    
	changeScale(cal1 cal){
		dialogsave=new Dialog(this,"Change Scale",true); 
		this.cal=cal;
	
		GridBagConstraints gbc;		
	
		dialogsave.setLayout(new GridBagLayout());
		cancel.addActionListener(this); 
		ok.addActionListener(this); 
		

		//Insertion into the Dialog

		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=0;
		dialogsave.add(label1, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=0;
		dialogsave.add(text_scale, gbc);

		//second line
	
		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=1;
		dialogsave.add(ok, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=1;
		dialogsave.add(cancel, gbc);

		dialogsave.pack();
		dialogsave.move(200,150);
		dialogsave.show();
		
	}
  
	public void actionPerformed(ActionEvent e){
		String STR=e.getActionCommand();
		if(STR.equals("Cancel")){	
			dialogsave.dispose();
	
		}
		
		if(STR.equals("OK")){	
			int sc=Integer.parseInt(text_scale.getText());
			
			if (sc!=0){
				cal.panelclient.scale=sc;
				cal.panelclient.length5=sc*cal.panelclient.length;
				cal.panelclient.temp5=sc*cal.panelclient.temp;
				cal.panelclient.repaint();
				cal.panelstatus.repaint();
				dialogsave.dispose();
			}
		}
	}
	
}

class Tool{

	int length=20;
	private int scale=1;
	private Graphics g;
	int counter=0;
	public int Xpos,Ypos;
	public int Xpos1,Ypos1;
	simulator pc;
	int changer;

	//Constructor
	Tool(Graphics g,simulator pc,int changer){
		this.g=g;
		this.pc=pc;
		this.changer=changer;

		
	}

	public void delay(int del){
	String str;
	for (int i=1;i<=del;i++)
		str="ovatsy"+"softwares";
	
}

	void moveLeft(){
		if(changer==0) pc.xmain--;
		drawTool(this.Xpos,this.Ypos,Color.black);
		for(int count=1;count<=scale;count++)
		{
			this.Xpos--;
			drawTool(this.Xpos,this.Ypos,Color.black);
		}
		drawTool(this.Xpos,this.Ypos,Color.white);

	}

	void moveRight(){
		if(changer==0) pc.xmain++;
		drawTool(this.Xpos,this.Ypos,Color.black);
		for(int count=1;count<=scale;count++)
		{	
			this.Xpos++;
			drawTool(this.Xpos,this.Ypos,Color.black);
		}
		drawTool(this.Xpos,this.Ypos,Color.white);
	}


	void moveUp(){
		if(changer==0) pc.ymain--;
		drawTool(this.Xpos,this.Ypos,Color.black);
		this.Ypos-=scale;
		drawTool(this.Xpos,this.Ypos,Color.white);
	}

	void moveDown(){
		if(changer==0) pc.ymain++;
		drawTool(this.Xpos,this.Ypos,Color.black);
		this.Ypos+=scale;
		drawTool(this.Xpos,this.Ypos,Color.white);
	}

	void draw(int Xpos,int Ypos,int temp1,int length,int scale){
		this.Xpos=(scale*Xpos)+40;
		this.Ypos=165+(Ypos-temp1)*scale;
		this.scale=scale;
		drawTool(this.Xpos,this.Ypos,Color.white);

	
	}

	private void drawTool(int Xpos,int Ypos,Color color){
		Color temp=g.getColor();
		g.setColor(color);
		g.drawLine(Xpos,Ypos-length,Xpos,Ypos);
		g.setColor(temp);

	}
}

class Rect{
 int upperX;
 int upperY;
 int lowerX;
 int lowerY;
 }


class Panelclient extends Panel implements ActionListener{
	
	public int noOfLines=0;
	public int paintStatus=0;
	public int greatflag;
	public int flip=0,counter=0;
	public int	maxcounter=0;
    public int lineX[]=new int[1000];
	public int lineY[]=new int[1000];
	public int lineupX[]=new int[1000];
	public int lineupY[]=new int[1000];
	public int flag=0;
	public int scale=1;
	public int temp,parameter,stln;
	public int length=20;
	public int length5,temp5;
	public int	global_int[]=new int[1000];
	public int simulate=0;

	Image	offScreenImage;
	Graphics G;

	public String filename="Noname"; 
	public String head=""; 
	public String temp_value=""; 
	
	public Dialog  dialog;
	public Dialog  dialogsave;
    
	char bufs[] = new char[50];
	char buf[] = new char[50]; 
	char rear[] = new char[50]; 
	char front[] = new char[50]; 
	
	cal1 cal;
	Frame mf;
	TextField text_name=new TextField(15);
	
	public Panelclient(int para_paint,int d,int l,String s,cal1 cal,Frame mf){
		//super();
        setBackground(Color.black);
		parameter=para_paint; 
		this.temp=d/2;
		this.length=l;
		this.cal=cal;
		this.mf=mf;
		
	}
	
	public void repaint1(int para_paint,int d,int l){
		
		this.parameter=para_paint; 
		
		if(d%2==1)
			greatflag=1;
		else greatflag=0; 
		
		this.temp=d/2;
		this.length=l;
       	this.temp5=temp*scale;
		this.length5=length*scale;

		noOfLines=0;
		flip=0;
		repaint();

	}


	public void reDraw(){
		
		int intrear=0;

		lineX[0]=40;
	    lineY[0]=165+temp;
		lineupX[0]=40;
	    lineupY[0]=165-temp;//MID OF THE AXIS
		flip=0;
				
		for(noOfLines=1;noOfLines<=counter;noOfLines++)
			try{
			intrear=global_int[noOfLines];
			if (flip==0){
				intrear=-intrear;
		 		if((lineY[noOfLines-1]+intrear)>=165+temp){
				lineY[noOfLines]=165+temp;
				lineupY[noOfLines]=165-temp;
				lineX[noOfLines]=lineX[noOfLines-1];
				lineupX[noOfLines]=lineupX[noOfLines-1];
				flip=1;
			}else{
				if((lineY[noOfLines-1]+intrear)<=165){
					lineY[noOfLines]=165;
					lineupY[noOfLines]=165;
					lineX[noOfLines]=lineX[noOfLines-1];
					lineupX[noOfLines]=lineupX[noOfLines-1];
					flip=1;
				}else{
					lineY[noOfLines]=lineY[noOfLines-1]+intrear;
					lineX[noOfLines]=lineX[noOfLines-1];
					lineupY[noOfLines]=lineupY[noOfLines-1]-intrear;
					lineupX[noOfLines]=lineupX[noOfLines-1];
		 			flip=1;
				}
			}
		 
			}else{//MAIN IF FLIP=1
				if((lineX[noOfLines-1]+intrear)>=20+length){
				lineX[noOfLines]=20+length;
				lineupX[noOfLines]=20+length;
				lineY[noOfLines]=lineY[noOfLines-1];
				lineupY[noOfLines]=lineupY[noOfLines-1];
				flip=0;
				}else{
					if((lineX[noOfLines-1]+intrear)<=40){
						lineX[noOfLines]=40;
						lineupX[noOfLines]=40;
						lineY[noOfLines]=lineY[noOfLines-1];
						lineupY[noOfLines]=lineupY[noOfLines-1];
						flip=0;
					}else{
						lineY[noOfLines]=lineY[noOfLines-1];
						lineX[noOfLines]=lineX[noOfLines-1]+intrear;
						lineupY[noOfLines]=lineupY[noOfLines-1];
						lineupX[noOfLines]=lineupX[noOfLines-1]+intrear;
						flip=0;
					}
				}
			}
		  	}catch(Exception e){
			}
			

		lineupY[noOfLines]=lineupY[noOfLines-1];
		lineupX[noOfLines]=	lineupX[noOfLines-1];
		lineY[noOfLines]=lineY[noOfLines-1];
		lineX[noOfLines]=lineX[noOfLines-1];
		
		repaint();
		noOfLines--;
	}

	public void cutLine(String s){
		
		int i=0,check=0,j,t=0,intrear=0;
		String temprear="";
		
		Frame f= new Frame();

		flag=0;
		stln=s.length();
		s.getChars(0,stln,bufs,0);
		do{
			front[i]=bufs[i];
			
			if((""+bufs[i]).equals(" "))
			    check=1;
			
			i++;
		}while(check!=1);

        
		for(j=i;j<stln;j++){
			t++;
			rear[t]=bufs[j];
			temprear+=rear[t];
		}
		
		lineX[0]=40;
	    lineY[0]=165+temp;
		lineupX[0]=40;
	    lineupY[0]=165-temp;//MID OF THE AXIS

		try{
			intrear=Integer.parseInt(temprear);
			counter++;
			maxcounter=counter;
			global_int[noOfLines+1]=intrear;
		 	noOfLines++;
			if (flip==0){
				intrear=-intrear;
		 	if((lineY[noOfLines-1]+intrear)>=165+temp){
				lineY[noOfLines]=165+temp;
				lineupY[noOfLines]=165-temp;
				lineX[noOfLines]=lineX[noOfLines-1];
				lineupX[noOfLines]=lineupX[noOfLines-1];
				flip=1;
			 }else{
				if((lineY[noOfLines-1]+intrear)<=165){
					lineY[noOfLines]=165;
					lineupY[noOfLines]=165;
					lineX[noOfLines]=lineX[noOfLines-1];
					lineupX[noOfLines]=lineupX[noOfLines-1];
					flip=1;
				}else{
					lineY[noOfLines]=lineY[noOfLines-1]+intrear;
					lineX[noOfLines]=lineX[noOfLines-1];
					lineupY[noOfLines]=lineupY[noOfLines-1]-intrear;
					lineupX[noOfLines]=lineupX[noOfLines-1];
		 			flip=1;
				}
			}
			
		 }else{//MAIN IF FLIP=1
			
			if(intrear>0){
				if((lineX[noOfLines-1]+intrear)>20+length){
				lineX[noOfLines]=20+length;
				lineupX[noOfLines]=22+length;
				lineY[noOfLines]=lineY[noOfLines-1];
				lineupY[noOfLines]=lineupY[noOfLines-1];
				flip=0;
			 }else{
				if((lineX[noOfLines-1]+intrear)<=40){
					lineX[noOfLines]=40;
					lineupX[noOfLines]=40;
					lineY[noOfLines]=lineY[noOfLines-1];
					lineupY[noOfLines]=lineupY[noOfLines-1];
					flip=0;
				}else{
					lineY[noOfLines]=lineY[noOfLines-1];
					lineX[noOfLines]=lineX[noOfLines-1]+intrear;
					lineupY[noOfLines]=lineupY[noOfLines-1];
					lineupX[noOfLines]=lineupX[noOfLines-1]+intrear;
					flip=0;
				}
			}
			}else{
				new message(f,"Parameter out of range!",true,"Parameter entered is greater than the size of the work-piece!");
				counter--;
				maxcounter=counter;
				noOfLines--;
				flag=1;
			}

		 }
		
		 if((lineupY[noOfLines]>=165)||(lineupX[noOfLines]>20+length)){
			 		
			 new message(f,"Parameter out of range!",true,"Parameter entered is greater than the size of the work-piece!");
			 noOfLines--;
			 counter--;
			 maxcounter=counter;
			 flag=1;
			 if(flip==1) flip=0;
			 else flip=1;

		 }

		 repaint();
			 		 
	}catch(Exception e){}
	
	if (counter>0)
		cal.undo.enable();
	
	if (maxcounter==counter)
		cal.redo.disable();
	
	cal.panelstatus.repaint();
	
}
public void sketchLine(int x1,int y1, int y2,Graphics g){
	g.drawLine((x1*scale)+40,165+(y1-temp)*scale,(x1*scale)+40,165+(y1-temp)*scale+y2);
}
public void delay(int del){
	String str;
	for (int i=1;i<=del;i++)
		str="ovatsy"+"softwares";
	
}


public void startSimulation(){
  	
		  simulate=1;
		  repaint();


}

public void update(Graphics g){
		Color col=g.getColor();
		g.setColor(Color.black);
		g.drawRect(0,0,400,400);
		g.setColor(col);
	 	offScreenImage=createImage(600,400);
	 	G=offScreenImage.getGraphics();
		
		int distance[]= new int[1000];		
		  		


	if (simulate==1) 
	{			
		
		simulator sim=new simulator(noOfLines+1,g,G,offScreenImage,this,temp,length,scale);
		sim.distance[1]=0;
		for(int i=1;i<=counter; i++)
			sim.distance[i+1]=global_int[i];
		
		paint(G);
		sim.start();
				
		cal.simulate==0;paintStatus=1;
				
	}
	else {
		paintStatus=0;
		paint(g);
	}

	simulate=0;
		if(paintStatus==1){

		 g.drawImage(offScreenImage,0,0,this);
		}


}


public void paint(Graphics g){   

	 Color col=g.getColor();
	 g.setColor(Color.black);
	 g.fillRect(0,0,600,400);
	 g.setColor(col);

	
	 g.setColor(Color.white);
	 for (int temp1=0;temp1<=2;temp1++)
		 g.drawRect(temp1,temp1,520-2*temp1,336-2*temp1);

		 if(parameter!=1){
			int xpts1[]={0,20,20,40,40,0,0};
			int ypts1[]={0-temp5,0-temp5,106-temp5,106-temp5,165-temp5,165-temp5,0-temp5};
			g.setColor(new Color(57,58,72));
			Polygon poly1=new Polygon(xpts1,ypts1,7);
			g.fillPolygon(poly1);
			
			int xpts2[]={0,40,40,20,20,0,0};
			int ypts2[]={165+temp5,165+temp5,224+temp5,224+temp5,335+temp5,335+temp5,165+temp5};
			Polygon poly2=new Polygon(xpts2,ypts2,7);
			g.fillPolygon(poly2);
        

			int xpts3[]={20,(length-20)*scale+40,(length-20)*scale+40,20,20};
			int ypts3[]={165-temp5,165-temp5,165+temp5,165+temp5,165-temp5};
			g.setColor(new Color(160,160,160));
			Polygon poly3=new Polygon(xpts3,ypts3,5);
			g.fillPolygon(poly3);
		
			int xpts4[]={20,(length-20)*scale+40,(length-20)*scale+40,20,20};
			int ypts4[]={165,165,165+temp5,165+temp5,165};
			g.setColor(new Color(128,128,128));
			Polygon poly4=new Polygon(xpts4,ypts4,5);
			g.fillPolygon(poly4);
		
			g.setColor(new Color(53,154,255));
			g.drawRect(20,165-temp5,(length-20)*scale+20,temp5*2);
       
			g.setColor(Color.white);
			for (int temp1=0;temp1<=2;temp1++)
				g.drawRect(temp1,temp1,520-2*temp1,336-2*temp1);
		
			g.setColor(new Color(255,255,0));
			g.drawLine(20,165-temp5,20,165+temp5);
			g.drawLine(20,165-temp5,40,165-temp5);
			g.drawLine(20,165+temp5,40,165+temp5);
			for (int temp2=1;temp2<=noOfLines;temp2++){
				g.drawLine(lineX[temp2]*scale-40*(scale-1),(165+(lineY[temp2]-165)*scale),lineX[temp2-1]*scale-40*(scale-1),(165+(lineY[temp2-1]-165)*scale));
				g.drawLine(lineupX[temp2]*scale-40*(scale-1),(165-(165-lineupY[temp2])*scale),lineupX[temp2-1]*scale-40*(scale-1),(165-(165-lineupY[temp2-1])*scale));
			}
		 }
		 if(parameter!=1){
			g.setColor(Color.white);
			g.drawString("Ref Pt.",5,165-temp5-5);
			g.drawOval(40-3,165-temp5-3,6,6);
			g.setColor(Color.red);
			lineupY[0]=165-temp;
			lineY[0]=165+temp;
			g.fillOval(lineupX[noOfLines]*scale-40*(scale-1)-2,(165-(165-lineupY[noOfLines])*scale)-2,4,4);
			g.fillOval(lineX[noOfLines]*scale-40*(scale-1)-2,(165+(lineY[noOfLines]-165)*scale)-2,4,4);
		 }

		 g.setColor(new Color(128,128,128));

		
		
	}

	
public void open(Frame fr){
	
	dialogsave=new Dialog(fr,"Open Work-Piece",true); 
    
	//Buttons
	
	Button cancel=new Button("Cancel");				
	Button openb=new Button("Open");

	//Label
	
	Label label1=new Label("File name ");
		
	GridBagConstraints gbc;		
	dialogsave.setLayout(new GridBagLayout());
	cancel.addActionListener(this); 
	openb.addActionListener(this); 
		
	//Insertion into the Dialog
	gbc = new GridBagConstraints();
	gbc.gridx =0;
	gbc.gridy=0;
	dialogsave.add(label1, gbc);

	gbc = new GridBagConstraints();
	gbc.gridx =1;
	gbc.gridy=0;
	dialogsave.add(text_name, gbc);

	//second line
	
	gbc = new GridBagConstraints();
	gbc.gridx =0;
	gbc.gridy=1;
	dialogsave.add(openb, gbc);

	gbc = new GridBagConstraints();
	gbc.gridx =1;
	gbc.gridy=1;
	dialogsave.add(cancel, gbc);

	dialogsave.pack();
	dialogsave.move(200,150);
	dialogsave.show();

}
		
public void save(Frame fr){

	dialogsave=new Dialog(fr,"Save Work-Piece",true); 
    
	//Buttons
	
	Button cancel=new Button("Cancel");				
	Button saveb=new Button("Save");

	//Label

	Label label1=new Label("File name ");
		
	GridBagConstraints gbc;		
	dialogsave.setLayout(new GridBagLayout());
	cancel.addActionListener(this); 
	saveb.addActionListener(this); 
		
	//Insertion into the Dialog

	gbc = new GridBagConstraints();
	gbc.gridx =0;
	gbc.gridy=0;
	dialogsave.add(label1, gbc);

	gbc = new GridBagConstraints();
	gbc.gridx =1;
	gbc.gridy=0;
	dialogsave.add(text_name, gbc);

	//second line
	
	gbc = new GridBagConstraints();
	gbc.gridx =0;
	gbc.gridy=1;
	dialogsave.add(saveb, gbc);

	gbc = new GridBagConstraints();
	gbc.gridx =1;
	gbc.gridy=1;
	dialogsave.add(cancel, gbc);

	dialogsave.pack();
	dialogsave.move(200,150);
	dialogsave.show();
	
}

public void actionPerformed(ActionEvent e){
	String STR=e.getActionCommand();
	if(STR.equals("Cancel")){	
		dialogsave.dispose();
	}
	
	if(STR.equals("Open")){	
		filename=text_name.getText();
		try{
			File testfile=new File(filename+".cal");
			RandomAccessFile Myfile=new RandomAccessFile(filename+".cal","r");
			if (testfile.exists()){
				cal.redo.disable();
				temp=Myfile.readShort();
				length=Myfile.readShort();
				counter=Myfile.readShort();
				
				if (counter>0)
					cal.undo.enable();
				
				if (counter%2==1) {
					cal.panelcommand.text_command.setText("Horizontal ");
					cal.panelcommand.front="Horizontal ";
				}else{
					cal.panelcommand.front="Vertical ";
					cal.panelcommand.text_command.setText("Vertical ");
				}

				maxcounter=counter;
				length5=length*scale;
				temp5=temp*scale;
				
				for(int i=1;i<=counter; i++)
					global_int[i]=Myfile.readShort();
				
				parameter=0;
				reDraw();
			  	mf.setTitle("Computer Aided Lathe - ["+filename+".cal]");
				cal.panelstatus.repaint();
				flag=0;
				paintStatus=0;		 
				repaint();
			}

		}catch(Exception ee){Frame f=new Frame();new message(f,"File not found !",true,"The file specified by you does not exists."); }	
		dialogsave.dispose();
	} 
		
	if(STR.equals("Save")){
		filename=text_name.getText();
		try{
			RandomAccessFile Myfile=new RandomAccessFile(filename+".cal","rw");
			Myfile.writeShort(temp);
			Myfile.writeShort(length);
			Myfile.writeShort(counter);
			
			for(int i=1;i<=counter; i++)
				Myfile.writeShort(global_int[i]);
			Myfile.close();
					
		}catch(Exception ee){ 
		}
		
		mf.setTitle("Computer Aided Lathe - ["+filename+".cal]");
		cal.panelstatus.repaint();
		dialogsave.dispose();
	}
		 
}
}

class Tempdialog implements ActionListener{
	 
	Dialog  dialog;
    Panelclient pc;
	Panelstatus ps;
	cal1 cal;

//Buttons
	
		Button cancel=new Button("Cancel");				
		Button enter=new Button("Enter");

//Label

		Label label1=new Label("Enter Diameter ");
		Label label2=new Label("Enter Length");
		
// TextField are here

		TextField text_dia=new TextField(8);
		TextField text_len=new TextField(8);
		
	Tempdialog(Panelclient pc,Frame fr,Panelstatus ps,cal1 cal)
	{
		
	    dialog=new Dialog(fr,"Work-piece Specification...",true); 
		GridBagConstraints gbc;		
		dialog.setLayout(new GridBagLayout());
		cancel.addActionListener(this); 
		enter.addActionListener(this); 
		enter.requestFocus();
		this.pc=pc;
		this.ps=ps;
		this.cal=cal;

//Insertion into the Dialog

		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=0;
		dialog.add(label1, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=0;
		gbc.anchor=GridBagConstraints.WEST;
		dialog.add(text_dia, gbc);


		
//second line
	
		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=1;
		dialog.add(label2, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=1;
		dialog.add(text_len, gbc);

//third line		

       	gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=2;
		dialog.add(enter, gbc);

		gbc = new GridBagConstraints();
		gbc.gridx =1;
		gbc.gridy=2;
		dialog.add(cancel, gbc);
		
		dialog.pack();
		dialog.move(200,150);
		dialog.show();
		
	
	}
	
  
	  

	
	public void actionPerformed(ActionEvent e){
		 String STR=e.getActionCommand();

		 if(STR.equals("Enter")){
			int dia=Integer.parseInt(text_dia.getText());
			int len=Integer.parseInt(text_len.getText());
			if((dia<251)&&(len<471)){
				cal.panelcommand.front="Vertical ";
				cal.panelcommand.text_command.setText("Vertical ");
				pc.flip=0;
				pc.repaint1(0,dia,len+20);
				dialog.dispose();

				
				pc.paintStatus=0;		 
				ps.repaint();

			}else {
				Frame f=new Frame();
				new message(f,"Specification out of range....",true,"The dimensions entered by can not be drawn in present scale!");
			}
         
		 }
		 
		 if(STR.equals("Cancel")){
			 dialog.dispose();
		 }
	}
}



class Panelcommand extends Panel {

TextField text_command=new TextField(42);
String str_command=new String("COMMAND");// LINE");
public String front="Vertical ";
char bufs[]=new char[500];
char rear[]=new char[500];
char front1[]=new char[500];

Panelclient pc;
int count=0;

	public Panelcommand(Panelclient pc)

	{
		super();
		this.pc=pc;
	  
		text_command.setText(front);
		
		setLayout(new GridBagLayout());
		GridBagConstraints gbc;		
		

		gbc = new GridBagConstraints();
		gbc.gridx =0;
		gbc.gridy=1;
		gbc.weightx=2;
	 	add(text_command, gbc);
		setFont(new Font("Helvetica",3,13));
		

	}

	public void paint(Graphics g){
		g.setColor(new Color(64,0,64));
		g.drawString(str_command,14,43);
		for(int i=1;i<=4;i++)
			g.drawLine(10,44+i,87,44+i);

	    
		g.setColor(Color.white); 
		for (int temp=0;temp<=2;temp++)
			g.drawRect(temp,temp+1,520-2*temp,73-2*temp);
	}

	public void flipCommand(){
		 String	s=text_command.getText();
		 char bufs[] = new char[2];
		 s.getChars(0,1,bufs,0);
		 if(bufs[0]=='H')
			 front="Horizontal ";
		 else 
			 front="Vertical ";

	}
	
	public boolean handleEvent(Event e){

	 if((e.id==e.KEY_PRESS) && (e.key==10)){
			 String s=text_command.getText();
			 int stln=s.length();
			 s.getChars(0,stln,bufs,0);

			int i=0,check=0;
			do{
				front1[i]=bufs[i];
				if((""+bufs[i]).equals(" "))
					check=1;
					i++;
			}while(check!=1);
			int j;
			String temprear="";
			int t=0;
			for(j=i;j<stln;j++){
				t++;
				rear[t]=bufs[j];
				temprear+=rear[t];
		}

		
			 
			 
			 
			 
			 int t1=Integer.parseInt(temprear);
			 
			 if ((t1!=0)||(front1[0]=='V')){
		 if (front.equals("Vertical ")) front="Horizontal ";
			  else front="Vertical ";
		 	 
              if((pc.lineX[pc.noOfLines]<pc.length+20)&&(pc.lineupX[pc.noOfLines]<pc.length+20))
				{pc.cutLine(s);
				if (pc.flag==1) flipCommand();
				text_command.setText(front); }
			  return true;
		 } }
	return false;
}
	
}

class Panelstatus extends Panel{

	Panelclient pc;
	Image backimg;

	public Panelstatus(Panelclient pc)
	{
		
		super();
		this.pc=pc;

		setFont(new Font("Helvetica",Font.BOLD,16));
		MediaTracker mt=new MediaTracker(this);
		backimg=Toolkit.getDefaultToolkit().getImage("backimg.jpg");
		try mt.waitForAll();catch(Exception e){}

	}
	public void paint(Graphics g){
		
		MediaTracker mt=new MediaTracker(this);
		g.drawImage(backimg,4,3,this);
		try mt.waitForAll();catch(Exception e){}
		pc.lineupX[0]=40;
		pc.lineupY[0]=165-pc.temp;
		
		g.setColor(Color.white); 
		for (int temp=0;temp<=2;temp++)
			g.drawLine(1,temp,108,temp);
		for (int temp=0;temp<=2;temp++)
			g.drawLine(temp+1,0,temp+1,600);
		for (int temp=0;temp<=2;temp++)
			g.drawLine(108+temp,0,108+temp,600);
		
		g.setColor(new Color(0,0,0));
		
		g.drawString("STATUS",22,30);
		for(int i=1;i<=4;i++)
			g.drawLine(10,33+i,99,33+i);
		g.setColor(new Color(64,0,64));
		g.drawString("Length",5,80);
		
		g.setColor(new Color(0,0,0));
		g.drawString(""+(pc.length-20),63,80);
		
		g.setColor(new Color(64,0,64));
		g.drawString("Dia",5,140);
		
		g.setColor(new Color(0,0,0));
		int a;
		if (pc.greatflag==1) a=pc.temp*2+1;else a=pc.temp*2;
		g.drawString(""+a,63,140);



		g.setColor(new Color(64,0,64));
		g.drawString("X Cod",5,260);
		
		g.setColor(new Color(0,0,0));
		g.drawString(""+(pc.lineupX[pc.noOfLines]-40),63,260);
						

		g.setColor(new Color(64,0,64));
		g.drawString("Y Cod",5,320);
		


		g.setColor(new Color(0,0,0));
		g.drawString(""+(-165+pc.temp+pc.lineupY[pc.noOfLines]),63,320);

				
		g.setColor(new Color(64,0,64));
		g.drawString("Scale",5,200);
		
		g.setColor(new Color(0,0,0));
		g.drawString(""+pc.scale,63,200);

	}

}


class message extends Dialog
 {


  private Button ok;
  private Label l_intro;

  public message(Frame a_frame, String the_title, boolean modal, String label)
   {
    super(a_frame, the_title, modal);
    l_intro = new Label();
    l_intro.setText((String)label);
    initial();
    resize(350,100);
    move(150,150);
    show();
    }

  void initial()
   {
     ok = new Button("OK");

     GridBagLayout gb = new GridBagLayout();
     GridBagConstraints gbc = new GridBagConstraints();
     gbc.fill = GridBagConstraints.NONE;
     gbc.anchor = GridBagConstraints.NORTH;
     gbc.weightx=.1;
     gbc.weighty=.2;
     setLayout(gb);
     gbc.gridwidth=GridBagConstraints.REMAINDER;
     gb.setConstraints(l_intro, gbc);
     add(l_intro);
     gb.setConstraints(ok, gbc);
     add(ok);
    }

  public boolean handleEvent(Event e)
   {
    if((e.id==Event.ACTION_EVENT) && (e.target.equals(ok)))
     {
	  //hide();	Ballo
	  setModal(false);
      dispose();
      return true;
      }
    else return false;
    }

 }

class Panelmain extends Panel{

	public Panelmain()
	{
		super();
	}
	public void paint(Graphics g){
		g.drawImage(Toolkit.getDefaultToolkit().getImage("backimg.jpg"),525,320,this);
		g.setColor(Color.white); 
		for (int temp=0;temp<=2;temp++)
			g.drawRect(523+temp,327,109-2*temp,85-temp);

		
		
	}



}


class simulator{

int distance[]=new int[1000];
private int rectcount=0;//To hold the total no. of cuttable rectangles.
private int flip=0;
private int i=0,j=0;
int xmain=0;
int ymain=0;
private int length=0;
private int radius=0;
private int multiplierv=1;//5;
private int multiplierh=1;
private int delv=20;
private int noOfLines=0;
int lent;


//Rect[] tempr;
//Rect[] rect;

int temprUpperX[]=new int[1000];
int temprUpperY[]=new int[1000];
int temprLowerX[]=new int[1000];
int temprLowerY[]=new int[1000];
int rectUpperX[]=new int[1000];
int rectUpperY[]=new int[1000];
int rectLowerX[]=new int[1000];
int rectLowerY[]=new int[1000];
 
 
private Stack stack=new Stack();//a stack used for sorting....




private Tool tool;
private Tool tool1;
private Graphics g;
private Panelclient pc;
private Image image;
int scale;

//Constructor

simulator(int noOfLines,Graphics g,Graphics G,Image image,Panelclient pc,int temp1,int length1,int scale1){
	tool1=new Tool(G,this,1);
	tool= new Tool(G,this,0);
	tool.draw(0,0,temp1,length1,scale1);	
	tool1.draw(0,2*temp1+tool.length/scale1,temp1,length1,scale1);	
	delay(10000);
	this.pc=pc;
	this.image=image;
	this.g=g;
	this.noOfLines=noOfLines;
	this.scale=scale1;
	this.lent=length1;
//	try{
//	rect=(Rect[])Array.newInstance(Class.forName("Rect"),1000);//=new Rect[100];
//	tempr=(Rect[])Array.newInstance(Class.forName("Rect"),1000);//=new Rect[100];
//	}catch(Exception e){System.out.println("la");}

}
	
void start(){
	makeRectangles(noOfLines);
	
	coalasceRectangles();
	sort();
	startMachining();

}



/*This method transforms descrete distances input raw, into rectangular
	 objects to be cut and stores them as array of the structure 'rect'. */

private void makeRectangles(int count){
	
 int x1;
 int x2,y;
try{
 x1=distance[1];
 y=distance[2];
}catch(Exception e){}
 for (int temp=3;temp<=count;temp++){
  if (odd(temp)==1){
	x2=x1+distance[temp];
	if(y!=0){
	 rectcount++;
	rectUpperX[rectcount]=x1;
	rectUpperY[rectcount]=0;
	rectLowerX[rectcount]=x2;
	rectLowerY[rectcount]=y;
	}
	x1=x2;
  }
  else y+=distance[temp];
 }		
}


/*This method coalasces rectangles to make bigger rectangles so that they can
 be cut in a much efficient way....*/


private void coalasceRectangles(){
 int i=0,j=i;   //Temporary variables.
 while(i<=rectcount){
  i++;
  j=i;
  while(j<=rectcount){
	j++;
	if (i!=j) coalasce(i,j);
  }
 }
}


/* This method checks the two rectangles with indices 'i' and 'j' if they
could be coalasced into bigger rectangles. If so, this method coalasces them
 (Hey! This method alters a global variable..!) and returns non zero value-
 otherwise returns zero*/


private int coalasce(int i,int j){
 int toBeReturned=0,temp1,temp2;
 if ((rectLowerX[i]==rectUpperX[j]) && (rectUpperY[i]==rectUpperY[j])){
  if (rectLowerY[i]>=rectLowerY[j]){
	temp1=rectLowerX[i];
	temp2=rectLowerY[i];
	rectLowerX[i]=rectLowerX[j];
	rectLowerY[i]=rectLowerY[j];
	rectLowerX[j]=temp1;
	rectLowerY[j]=temp2;
	rectUpperX[j]=rectUpperX[i];
	rectUpperY[j]=rectLowerY[i];
  }
  else{
	temp1=rectLowerX[i];
	temp2=rectLowerY[i];
	rectLowerX[i]=rectLowerX[j];
	rectUpperX[j]=temp1;
	rectUpperY[j]=temp2;
	}
  toBeReturned=1;
  }

 if ((rectLowerX[j]==rectUpperX[i]) && (rectUpperY[j]==rectUpperY[i])){
  if (rectLowerY[j]>=rectLowerY[i]){
	temp1=rectLowerX[j];
	temp2=rectLowerY[j];
	rectLowerX[j]=rectLowerX[i];
	rectLowerY[j]=rectLowerY[i];
	rectLowerX[i]=temp1;
	rectLowerY[i]=temp2;
	rectUpperX[i]=rectUpperX[j];
	rectUpperY[i]=rectLowerY[j];
  }
  else{
	temp1=rectLowerX[j];
	temp2=rectLowerY[j];
	rectLowerX[j]=rectLowerX[i];
	rectUpperX[i]=temp1;
	rectUpperY[i]=temp2;
	}
  toBeReturned=1;
  }
//	for(int i1=1;i1<=rectcount;i1++)
 //	System.out.println("Made rect :"+rectUpperX[i1]+" "+rectUpperY[i1]+" "+rectLowerX[i1]+" "+rectLowerY[i1]);
  return toBeReturned;

}

//Tells if 'temp' is odd .

private int odd(int temp){
 if (temp%2==0) return 0;else return 1;
}

public void delay(int del){
	String str;
	for (int i=1;i<=del;i++)
		str="ovatsy"+"softwares";
	
}



// This method sorts the rectangles in an order such that they can be cut
// in the most efficient manner on the machine.....

private void sort(){

 int counter=0;
 int count=0;
 for (int temp=1;temp<=rectcount;temp++){
  if (rectUpperY[temp]==0){
	count++;
	stack.push(temp);
  }
 }
 while(stack.empty()==0){
  counter++;
  int p=stack.poped();
  temprUpperX[counter]=rectUpperX[p];
  temprUpperY[counter]=rectUpperY[p];
  temprLowerX[counter]=rectLowerX[p];
  temprLowerY[counter]=rectLowerY[p];

  for (int t=1;t<=rectcount;t++)
  if(t!=p)
  {
	if ((rectUpperY[t]==rectLowerY[p])
	 &&(rectUpperX[t]<=rectLowerX[p])
	 &&(rectUpperX[t]>=rectUpperX[p]))
	  stack.push(t);
  }
  rectUpperX[p]=counter*-1;
  rectUpperY[p]=counter*-1;
  rectLowerX[p]=counter*-1;
  rectLowerY[p]=counter*-1;

 }
 rectcount=counter;
}

/***************************************************************************/

/* The code here downwards is devoted to machining the rectangles formed in
 array 'rect' on the Lathe. */



private void startMachining(){
//tool1.Ypos=100+20*radius+10;

for (int count=1;count<=rectcount;count++)
 if ((temprUpperY[count]!=temprLowerY[count])){
  cutRectangle(temprUpperX[count],temprUpperY[count],temprLowerX[count],temprLowerY[count]);
  }
delay(500);
moveTo(0,0,20);

}


private void cutRectangle(int x1,int y1,int x2,int y2){

	
	int x=x1,y=y1;
	moveTo(x1,y1,2);

	while (y!=y2+1){

	 moveTo(x,y,(scale)*(7000/lent));

	 if (x==x1) x=x2;else x=x1;
	 y++;
	}
}

private void moveTo(int x,int y,int del){
 if (ymain<y){
	 while(ymain!=y){ 
		 tool.moveDown();
		 tool1.moveUp();
		 delay(del);
		 g.drawImage(image,0,0,pc);
	 }
}else {
	 while(ymain!=y) {
		 tool.moveUp();
		 tool1.moveDown();
		 delay(del);
		 g.drawImage(image,0,0,pc);
	 }
}

 if (xmain<x) {
	 while(xmain!=x){
		 tool.moveRight();
		 tool1.moveRight();
		 delay(del);
		 g.drawImage(image,0,0,pc);
	 }
}else {
	 while(xmain!=x){
		 tool.moveLeft();
		 tool1.moveLeft();
		 delay(del);
		 g.drawImage(image,0,0,pc);
	 }
}

}



}

 class Stack{
 private int count;
 private int rect1[]=new int[1000];


 int empty(){
  if (count==0) return 1;else return 0;
 }

 void push(int rect){
  count++;
  rect1[count]=rect;
 }

 //constructor
 Stack(){
 count=0;
 }

 int poped(){
	int temp=rect1[count];
	count--;
	return temp;
 }
}


class OnHelpAbout extends Dialog {
	Button TEAM;
	int resizer=0;
	int first=0;
	Image img;
	
	
	OnHelpAbout(Frame f){
	
		super(f,true);
		

		setTitle("About Computer Aided Lathe.");
		setBackground(Color.lightGray);
		setResizable(false);
		setFont(new Font("Helvetica",Font.BOLD,11));
		About about=new About(this);
		about.CreateControls();
		
		TEAM=new Button("TEAM");
		add(TEAM);
		int startx=size().width/2-10  ;
		about.m_Layout.setShape(TEAM,130,58,40,12);
		move(160,150);
		show();
	}
	public void  paint(Graphics g){
		if (first==0){
			first=1;
			img=Toolkit.getDefaultToolkit().getImage("heart.gif");
			while(img==null){}
		}
		g.drawImage(img,230,20,this);
		g.setColor(new Color(29,29,29));
		g.drawLine(18,79,190,79);

		g.setColor(new Color(255,255,255));
		g.drawLine(18,80,190,80);

		g.setColor(new Color(29,29,29));
		g.drawLine(18,124,314,124);

		g.setColor(new Color(255,255,255));
		g.drawLine(18,125,314,125);
		g.setColor(Color.red);
		g.drawString("Developed By :",10,165);
		g.setColor(Color.green);
		g.drawString(" m Prakash Kanwar",16,190);
		g.drawString(" ijay Sekhri",16,205);
		g.drawString(" nuj Sahni",15,220);
		g.drawString(" arun Nagpal",13,235);
		g.drawString(" unil Sharma",15,250);
		g.drawString(" ogesh Dhingra",13,265);

		g.setColor(new Color(0,75,0));
		g.drawString("O",10,190);
		g.drawString("V",10,205);
		g.drawString("A",10,220);
		g.drawString("T",10,235);
		g.drawString("S",10,250);
		g.drawString("Y",10,265);


	}


	public boolean handleEvent(Event e){

	 if(e.id==201){
		setModal(false);
		dispose();
		return true;
	 } 

	 if((e.id==Event.ACTION_EVENT)&&(e.target.equals(TEAM))&&(resizer==0)){
		 resizer=1;
		 resize(size().width,size().height+130);
		 repaint();
		return true;
	 }

	return false;
	}
}



class OnHelpContents extends Dialog {
	Button OK;
	Graphics G;
	Image image;
	int first=0;
	
	OnHelpContents(Frame f){
	
		super(f,true);
		
		setTitle("Help Contents");
		setResizable(false);
		setFont(new Font("Helvetica",Font.BOLD,11));
		
		resize(400,150);
		move(110,170);
		show();
	}

   private void delay(int del){
	   for(int count=0;count<=del;count++)
	    String success="OVATSY"+"Softwares";
   }

   public void paint(Graphics g){
	   g.fillRect(0,0,400,150);

	   if (first==0){
		
		image=createImage(400,80);
	    G=image.getGraphics();
		G.setColor(Color.red);
		G.setFont(new Font("Helvetica",Font.BOLD,18));
		G.drawString("Computer Aided Lathe Ver 1.00",10,15);
		G.setFont(new Font("Helvetica",Font.PLAIN,12));
		G.drawString("For technical details about the software see readme.txt in the",10,40);
		G.drawString("    installation directory.",10,55);
		for (int f=0;f<=30;f++){
		 g.fillRect(0,0,400,70);
		 g.drawImage(image,0,f+10,this);
		 delay(100);
		
		 
		}
	   }
	    if (first==1) g.drawImage(image,0,40,this);

	   if (first==0) first=1;
	  

	   
	 
   }

   public boolean handleEvent(Event e){
	   if(e.id==201){
		   setModal(false);
		   dispose();
		   return true;
	   }
	   return false;
   }

}
