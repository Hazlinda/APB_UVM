# APB_UVM

# APB.sv

    `ifndef APB__SV
    `define APB__SV

    `include "apb_if.sv"
    `include "uvm_macros.svh"

    package apb_pkg; 

    import uvm_pkg::*;

    typedef virtual apb_if apb_vif; // Declare an alias for this long definition

    typedef class apb_agent; // Declare an alias for this long definition

    `include "apb_rw.sv"
    `include "apb_config.sv"
    `include "apb_master.sv"
    `include "apb_monitor.sv"
    `include "apb_sequencer.sv"
    `include "apb_agent.sv"
    endpackage

    `endif

# APB_agent.sv (Agent)

    `ifndef APB_AGENT__SV
    `define APB_AGENT__SV


    typedef class apb_agent;


    class apb_agent extends uvm_agent;

      apb_sequencer sqr; //this agent is active type because it have 3 component if only have monitor then it pasive agent
      apb_master    drv;
      apb_monitor   mon;

      apb_vif       vif;

      `uvm_component_utils_begin(apb_agent) //uvm_component_utils begin and end is define when need to declare uvm_field_object intermidiate 
      `uvm_field_object(sqr, UVM_ALL_ON)
      `uvm_field_object(drv, UVM_ALL_ON)
      `uvm_field_object(mon, UVM_ALL_ON)
      `uvm_component_utils_end
   
      function new(string name, uvm_component parent = null);
        super.new(name, parent);
      endfunction

      virtual function void build_phase(uvm_phase phase);
        sqr = apb_sequencer::type_id::create("sqr", this); //create object of class 
        drv = apb_master::type_id::create("drv", this); //use type_id:: because we can overide that class without changing single line
        mon = apb_monitor::type_id::create("mon", this);
      
        if (!uvm_config_db#(apb_vif)::get(this, "", "vif", vif)) begin // Retrieve the string that was set in config_db from the test class
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this agent instance")
        end
      endfunction: build_phase

      virtual function void connect_phase(uvm_phase phase); //connect phases 
          drv.seq_item_port.connect(sqr.seq_item_export);//seq_item_export instantiate to sqr and connected driver through the driver
      endfunction
    endclass: apb_agent

    `endif
    
# APB_config.sv (Config)

    `ifndef APB_AGENT__SV
    `define APB_AGENT__SV


    typedef class apb_agent;


    class apb_agent extends uvm_agent;

      apb_sequencer sqr; //this agent is active type because it have 3 component if only have monitor then it pasive agent
      apb_master    drv;
      apb_monitor   mon;

      apb_vif       vif;

      `uvm_component_utils_begin(apb_agent) //uvm_component_utils begin and end is define when need to declare uvm_field_object intermidiate 
      `uvm_field_object(sqr, UVM_ALL_ON)
      `uvm_field_object(drv, UVM_ALL_ON)
      `uvm_field_object(mon, UVM_ALL_ON)
      `uvm_component_utils_end
   
      function new(string name, uvm_component parent = null);
          super.new(name, parent);
      endfunction

      virtual function void build_phase(uvm_phase phase);
          sqr = apb_sequencer::type_id::create("sqr", this); //create object of class 
          drv = apb_master::type_id::create("drv", this); //use type_id:: because we can overide that class without changing single line
          mon = apb_monitor::type_id::create("mon", this);
      
          if (!uvm_config_db#(apb_vif)::get(this, "", "vif", vif)) begin // Retrieve the string that was set in config_db from the test class
          `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this agent instance")
        end
      endfunction: build_phase

      virtual function void connect_phase(uvm_phase phase); //connect phases 
          drv.seq_item_port.connect(sqr.seq_item_export);//seq_item_export instantiate to sqr and connected driver through the driver
      endfunction
    endclass: apb_agent

    `endif
    
 
 # APB_if.sv (Interface)
 
    `ifndef APB_IF__SV
    `define APB_IF__SV

    `timescale 1ns/1ns

    interface apb_if(input bit pclk);
      wire [31:0] paddr;
      wire        psel;
      wire        penable;
      wire        pwrite;
      wire [31:0] prdata;
      wire [31:0] pwdata;


      clocking mck @(posedge pclk); //master clocking 
          output paddr, psel, penable, pwrite, pwdata;
          input  prdata; 

          sequence at_posedge;
            1;
          endsequence : at_posedge
      endclocking: mck

      clocking sck @(posedge pclk); //slave clocking
          input  paddr, psel, penable, pwrite, pwdata;
          output prdata;

          sequence at_posedge_; // FIXME todo review 
            1;
          endsequence : at_posedge_
      endclocking: sck

      clocking pck @(posedge pclk); //passive clocking
          input paddr, psel, penable, pwrite, prdata, pwdata; //all the signal is input
      endclocking: pck 

      modport master(clocking mck);
      modport slave(clocking sck);
      modport passive(clocking pck);

    endinterface: apb_if

    `endif

# APB_master.sv (Master)

    `ifndef APB_MASTER__SV
    `define APB_MASTER__SV

    typedef class apb_master;
    class apb_master_cbs extends uvm_callback;
        virtual task trans_received (apb_master xactor , apb_rw cycle);endtask
        virtual task trans_executed (apb_master xactor , apb_rw cycle);endtask
    endclass

    class apb_master extends uvm_driver#(apb_rw);

        `uvm_component_utils(apb_master)
  
      event trig;
      apb_vif sigs;
      apb_config cfg;

      function new(string name,uvm_component parent = null);
          super.new(name,parent);
      endfunction
   
      virtual function void build_phase(uvm_phase phase);
          apb_agent agent;
          if ($cast(agent, get_parent()) && agent != null) begin
            sigs = agent.vif;
          end
          else begin
            if (!uvm_config_db#(apb_vif)::get(this, "", "vif", sigs)) begin
                `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this driver instance")
            end
          end
      endfunction

      virtual task run_phase(uvm_phase phase);
          super.run_phase(phase);

          this.sigs.mck.psel    <= '0;
          this.sigs.mck.penable <= '0;

          forever begin
            apb_rw tr;
            @ (this.sigs.mck);

            seq_item_port.get_next_item(tr);

            // TODO: QUESTA issue with hier ref to sequence via modport; need workaround?
    `ifdef VCS
            if (!this.sigs.mck.at_posedge.triggered)
    `endif

    `ifdef INCA
              // FIXME      if (!this.sigs.mck.at_posedge.triggered) // this is wrong and has to be reviewed
    `endif
    `ifdef QUESTA	
	       if (!this.sigs.mck.triggered)
    `endif
	       @ (this.sigs.mck);
         
            this.trans_received(tr);
             `uvm_do_callbacks(apb_master,apb_master_cbs,trans_received(this,tr))
         
	      case (tr.kind)
              apb_rw::READ:  this.read(tr.addr, tr.data);  
              apb_rw::WRITE: this.write(tr.addr, tr.data);
            endcase
         
            this.trans_executed(tr);
            `uvm_do_callbacks(apb_master,apb_master_cbs,trans_executed(this,tr))

            seq_item_port.item_done();
       ->trig ;
          end
      endtask: run_phase

      virtual protected task read(input  bit   [31:0] addr,
                                  output logic [31:0] data);

          this.sigs.mck.paddr   <= addr;
          this.sigs.mck.pwrite  <= '0;
          this.sigs.mck.psel    <= '1;
          @ (this.sigs.mck);
          this.sigs.mck.penable <= '1;
          @ (this.sigs.mck);
          data = this.sigs.mck.prdata;
          this.sigs.mck.psel    <= '0;
          this.sigs.mck.penable <= '0;
      endtask: read

      virtual protected task write(input bit [31:0] addr,
                                   input bit [31:0] data);
          this.sigs.mck.paddr   <= addr;
          this.sigs.mck.pwdata  <= data;
          this.sigs.mck.pwrite  <= '1;
          this.sigs.mck.psel    <= '1;
          @ (this.sigs.mck);
          this.sigs.mck.penable <= '1;
          @ (this.sigs.mck);
          this.sigs.mck.psel    <= '0;
          this.sigs.mck.penable <= '0;
      endtask: write

      virtual protected task trans_received(apb_rw tr);
      endtask
 
      virtual protected task trans_executed(apb_rw tr);
      endtask
    endclass: apb_master

    `endif
    
# APB_monitor.sv (Monitor)

	`ifndef APB_MONITOR__SV
	`define APB_MONITOR__SV

	typedef class apb_monitor;

	class apb_monitor_cbs extends uvm_callback;
  	  virtual function void trans_observed(apb_monitor xactor,apb_rw cycle);
  	  endfunction:trans_observed
	endclass: apb_monitor_cbs


	class apb_monitor extends uvm_monitor;
   	  virtual apb_if.passive sigs;

   	uvm_analysis_port#(apb_rw) ap;
   	apb_config cfg;

   	//`uvm_component_utils(apb_monitor)

   	`uvm_component_utils_begin(apb_monitor)
	//   `uvm_object_utils(cfg)
   	`uvm_component_utils_end


   	function new(string name, uvm_component parent = null);
      	  super.new(name, parent);
      	  ap = new("ap", this);
   	endfunction: new

   	virtual function void build_phase(uvm_phase phase);
      	  apb_agent agent;
      	  if ($cast(agent, get_parent()) && agent != null) begin
         sigs = agent.vif;
      	end
      	else begin
            virtual apb_if tmp;
         	if (!uvm_config_db#(apb_vif)::get(this, "", "vif", tmp)) begin
            `uvm_fatal("APB/MON/NOVIF", "No virtual interface specified for this monitor instance")
         end
         	sigs = tmp;
      	end
   	endfunction

   	virtual task run_phase(uvm_phase phase);
      	   super.run_phase(phase);
      	   forever begin
         apb_rw tr;
         
         // Wait for a SETUP cycle
         do begin
            @ (this.sigs.pck);
         end
         while (this.sigs.pck.psel !== 1'b1 ||
                this.sigs.pck.penable !== 1'b0);

         tr = apb_rw::type_id::create("tr", this);
         
         tr.kind = (this.sigs.pck.pwrite) ? apb_rw::WRITE : apb_rw::READ;
         tr.addr = this.sigs.pck.paddr;

         @ (this.sigs.pck);
         if (this.sigs.pck.penable !== 1'b1) begin
            `uvm_error("APB", "APB protocol violation: SETUP cycle not followed by ENABLE cycle");
         end
         tr.data = (tr.kind == apb_rw::READ) ? this.sigs.pck.prdata :
                                               this.sigs.pck.pwdata;

         trans_observed(tr);
         `uvm_do_callbacks(apb_monitor,apb_monitor_cbs,trans_observed(this,tr))

           ap.write(tr);
      	end
   	endtask: run_phase

   	virtual protected task trans_observed(apb_rw tr);
   	endtask

	endclass: apb_monitor

	`endif


# APB_rw.sv (Read Write)

	`ifndef APB_RW__SV
	`define APB_RW__SV

	class apb_rw extends uvm_sequence_item;
  
   	typedef enum {READ, WRITE} kind_e;
   	rand bit   [31:0] addr;
   	rand logic [31:0] data;
   	rand kind_e kind;  
 
   	`uvm_object_utils_begin(apb_rw)
     	`uvm_field_int(addr, UVM_ALL_ON | UVM_NOPACK);
     	`uvm_field_int(data, UVM_ALL_ON | UVM_NOPACK);
     	`uvm_field_enum(kind_e,kind, UVM_ALL_ON | UVM_NOPACK);
   	`uvm_object_utils_end
   
   	function new (string name = "apb_rw");
           super.new(name);
        endfunction

   	function string convert2string();
     	   return $sformatf("kind=%s addr=%0h data=%0h",kind,addr,data);
   	endfunction

	endclass: apb_rw


	class reg2apb_adapter extends uvm_reg_adapter;

  	`uvm_object_utils(reg2apb_adapter)

   	function new(string name = "reg2apb_adapter");
      		super.new(name);
   	endfunction 

  	virtual function uvm_sequence_item reg2bus(const ref uvm_reg_bus_op rw);
    	  apb_rw apb = apb_rw::type_id::create("apb_rw");
    	  apb.kind = (rw.kind == UVM_READ) ? apb_rw::READ : apb_rw::WRITE;
    	  apb.addr = rw.addr;
    	  apb.data = rw.data;
    	  return apb;
  	endfunction

  	virtual function void bus2reg(uvm_sequence_item bus_item,
           ref uvm_reg_bus_op rw);
    	apb_rw apb;
    	if (!$cast(apb,bus_item)) begin
      	`uvm_fatal("NOT_APB_TYPE","Provided bus_item is not of the correct type")
      	return;
    	end
          rw.kind = apb.kind == apb_rw::READ ? UVM_READ : UVM_WRITE;
          rw.addr = apb.addr;
          rw.data = apb.data;
    	  rw.status = UVM_IS_OK;
  	endfunction

	endclass
	`endif


# apb_sequencer.sv (Sequencer)

	`ifndef APB_SEQUENCER__SV
	`define APB_SEQUENCER__SV

	class apb_sequencer extends uvm_sequencer #(apb_rw);

  	 `uvm_component_utils(apb_sequencer)

   	function new(input string name, uvm_component parent=null);
      	   super.new(name, parent);
   	endfunction : new

	endclass : apb_sequencer

	`endif


 
