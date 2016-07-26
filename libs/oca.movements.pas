unit oca.movements;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tKey           = integer;
  tOcaMovement   = record
                     number : tKey;
                     next   : idxRange;
                   end;
  tControlRecord = record
                     first  : idxRange;
                     last   : idxRange;
                     erased : idxRange;
                     count  : integer;              
                   end;
  tControl       =  file of tControlRecord;
  tData          = file of tOcaMovement;
  tQueueOcaMvmt  = record
                     data    : tData;
                     control : tControl;
                   end;

//(var this : tQueueOcaMvmt);
  procedure newEmptyStack (var this : tQueueOcaMvmt; path, filename : string);
  procedure insert        (var this : tQueueOcaMvmt; item : tOcaMovement);
  procedure deletePos     (var this : tQueueOcaMvmt; pos : idxRange);
  procedure deleteItem    (var this : tQueueOcaMvmt; item : tOcaMovement);

  function  get           (var this : tQueueOcaMvmt; pos : idxRange) : tOcaMovement;
  function  isEmpty       (var this : tQueueOcaMvmt) : boolean;
  function  length        (var this : tQueueOcaMvmt) : integer;
  function  first         (var this : tQueueOcaMvmt) : idxRange;
  function  last          (var this : tQueueOcaMvmt) : idxRange;
  function  next          (var this : tQueueOcaMvmt; pos : idxRange) : idxRange;
  function  search        (var this : tQueueOcaMvmt; key : tKey; var pos : idxRange) : boolean;

  function  isValidPos    (var this : tQueueOcaMvmt; pos : idxRange) : boolean;

implementation

function getControlRecord(var this : tQueueOcaMvmt) : tControlRecord;
var 
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tQueueOcaMvmt; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

procedure newEmptyStack (var this : tQueueOcaMvmt; path, filename : string);
var
  fullFileName : string;
  Rc           : tControlRecord;
begin
  fullFileName := path + filename;

  //check if data file exists
  assign(this.data, fullFileName + '.dat');
  if not fileexists(fullFileName + '.dat') then
    rewrite(this.data)
  else
    reset(this.data);
  close(this.data);

  //check if data file exists
  assign(this.control, fullFileName + '.ctrl');
  if not fileexists(fullFileName + '.ctrl') then
    begin
      rewrite(this.control);
      Rc.first  := NULLIDX;
      Rc.last   := NULLIDX;
      Rc.erased := NULLIDX;
      Rc.count  := 0;
      seek(this.control, 0);
      write(this.control, Rc);
    end
  else
    reset(this.control);
  close(this.control);
end;

function  isEmpty (var this : tQueueOcaMvmt) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.count = 0;
end;

function  length (var this : tQueueOcaMvmt) : integer;
var
  Rc : tControlRecord;
begin
  Rc     := getControlRecord(this);
  length := Rc.count;
end;

function  first (var this : tQueueOcaMvmt) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc    := getControlRecord(this);
  first := Rc.first;
end;

function  last (var this : tQueueOcaMvmt) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc   := getControlRecord(this);
  last := Rc.last;
end;

function  get (var this : tQueueOcaMvmt; pos : idxRange) : tOcaMovement;
var
  item: tOcaMovement;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  get := item;
end;

procedure update (var this : tQueueOcaMvmt; pos : idxRange; var item : tOcaMovement);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

function append (var this : tQueueOcaMvmt; var item : tOcaMovement) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaMovement;
begin
  Rc := getControlRecord(this);
  if Rc.erased = NULLIDX then
    begin
      reset(this.data);
      seek (this.data, FileSize(this.data));
      write(this.data, item);
      close(this.data);
    end
  else
    begin
      pos       := Rc.erased;
      auxItem   := get(this, Rc.erased);
      Rc.erased := auxItem.next;

      update(this, pos, item);      

      setControlRecord(this, Rc);
    end;
  append := pos;
end;

function  next (var this : tQueueOcaMvmt; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord;
  Ridx : idxRange;
  item : tOcaMovement;
begin
  if pos = NULLIDX then
    begin
      Rc   := getControlRecord(this);
      next := Rc.first;
    end
  else
    begin
      item := get(this, pos);
      next := item.next;  
    end;  
end;

function  search (var this : tQueueOcaMvmt; key : tKey; var pos : idxRange) : boolean;
var
  found : boolean;
  Rc    : tControlRecord;
  Ridx  : idxRange; 
  item  : tOcaMovement;
begin
  found := false;
  Rc    := getControlRecord(this);
  pos   := NULLIDX;
  if Rc.first <> Rc.last then
    begin
      repeat
        Ridx := next(this, pos);
        item := get(this, Ridx);
        if item.number = key then
          found := true
        else
          begin
            pos := Ridx;          
          end;
      until found or (Ridx = Rc.last) or (item.number > key);
    end;
  
  if found then pos := Ridx;

  search := found;
end;

procedure insert (var this : tQueueOcaMvmt; item : tOcaMovement);
var
  pos, auxPos : idxRange;
  auxItem     : tOcaMovement;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if Rc.first = Rc.last then
    begin
      auxPos   := append(this, item);
      Rc.first := auxPos;
      Rc.last  := auxPos;
      Rc.count := 1;
      setControlRecord(this, Rc);
    end
  else  
    if not search(this, item.number, pos) then
      begin      
        auxPos  := append(this, item);

        if pos = NULLIDX then
          Rc.first := auxPos;

        if pos = Rc.last then
          setControlRecord(this, Rc);

        auxItem := get(this, pos);
        pos     := auxItem.next;

        auxItem.next := auxPos;
        update(this, auxPos, auxItem);

        item.next := pos;
        update(this, pos, item);   

        Rc.count := Rc.count + 1;
        setControlRecord(this, Rc);   
      end;
end;

procedure deletePos (var this : tQueueOcaMvmt; pos : idxRange);
begin
  
end;

procedure deleteItem (var this : tQueueOcaMvmt; item : tOcaMovement);
begin
  
end;

function  isValidPos (var this : tQueueOcaMvmt; pos : idxRange) : Boolean;
begin
  isValidPos := pos <> NULLIDX;
end;

end.