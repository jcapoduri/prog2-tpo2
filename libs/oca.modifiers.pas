unit oca.modifiers;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tModifiers     = (Goose, Dice, Bridge, Prison, Inn, Pit, Labyrinth, Death);
  tOcaModifier    = record
                     modifier : tModifiers;
                     cell     : integer;  
                     next     : idxRange;
                   end;
  tControlRecord = record
                     first  : idxRange;
                     erased : idxRange;
                   end;
  tControl       = file of tControlRecord;
  tData          = file of tOcaModifier;
  tStackOca      = record
                     data    : tData;
                     control : tControl;
                   end;

  procedure newEmptyStack    (var this : tStackOca; path, filename : string);
  procedure push             (var this : tStackOca; item : tOcaModifier);
  function  peek             (var this : tStackOca) : tOcaModifier;
  function  pop              (var this : tStackOca) : tOcaModifier;
  function  isEmpty          (var this : tStackOca) : boolean;
  function  search           (var this : tStackOca; key : tModifiers; var pos : idxRange) : boolean;
  function  existsCell       (var this : tStackOca; cell: integer) : boolean;
  function  generateModifier (var this : tStackOca;  modifier : tModifiers; cell: integer) : tOcaModifier;


implementation

function getControlRecord(var this : tStackOca) : tControlRecord;
var 
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tStackOca; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
end;

procedure newEmptyStack (var this : tStackOca; path, filename : string);
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
      Rc.erased := NULLIDX;
      seek(this.control, 0);
      write(this.control, Rc);
    end
  else
    reset(this.control);
  close(this.control);
end;

function  isEmpty (var this : tStackOca) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.first = NULLIDX;
end;

// inner function
function  get (var this : tStackOca; pos : idxRange) : tOcaModifier;
var
  item: tOcaModifier;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  get := item;
end;

procedure update (var this : tStackOca; pos : idxRange; var item : tOcaModifier);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

function append (var this : tStackOca; var item : tOcaModifier) : idxRange;
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaModifier;
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

// public functions
procedure push (var this : tStackOca; item : tOcaModifier);
var
  auxPos : idxRange;
  Rc     : tControlRecord;
begin
  Rc := getControlRecord(this);
  if Rc.first = NULLIDX then
    begin
      auxPos   := append(this, item);
      Rc.first := auxPos;
      setControlRecord(this, Rc);
    end
  else  
    begin      
      auxPos      := append(this, item);
      item.next   := Rc.first;
      Rc.first    := auxPos; 
      update(this, auxPos, item);
      setControlRecord(this, Rc);   
    end;
end;

function  peek (var this : tStackOca) : tOcaModifier;
var
  pos, auxPos : idxRange;
  auxItem     : tOcaModifier;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if not (Rc.first = NULLIDX) then
    begin
      auxItem := get(this, Rc.first); 
    end; 
  peek := auxItem;
end;

function  pop (var this : tStackOca) : tOcaModifier;
var
  pos, auxPos : idxRange;
  auxItem     : tOcaModifier;
  Rc          : tControlRecord;
begin
  Rc := getControlRecord(this);
  if not (Rc.first = NULLIDX) then
    begin
      
    end;
end;

function  search (var this : tStackOca; key : tModifiers; var pos : idxRange) : boolean;
begin
  search := true
end;

function  existsCell (var this : tStackOca; cell: integer) : boolean;
var
  item: tOcaModifier;
begin
  if isEmpty(this) then
    existsCell := false
  else
    begin
      item := pop(this);
      if (item.cell = cell) then
        existsCell := true
      else
        existsCell := false;
    end;
end;

function  generateModifier (var this : tStackOca;  modifier : tModifiers; cell: integer) : tOcaModifier;
var 
  item: tOcaModifier;
begin
  item.modifier    := modifier;
  item.cell        := cell;
  item.next        := NULLIDX;
  generateModifier := item;
end;

end.