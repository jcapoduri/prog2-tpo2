unit oca.space.list;

interface

uses
  oca.space,
  sysutils;

const
  NULLIDX  = -1;

type
  tModel         = tOcaSpace;
  idxRange       = NULLIDX..MAXINT;
  tKey           = integer;
  tControlRecord = record
                     first  : idxRange;
                     last   : idxRange;
                     erased : idxRange;
                     count  : Integer;              
                   end;
  tIdxRecord     = record
                     key  : tKey;
                     next : idxRange;                      
                   end;
  tControl       = record
                     control : file of tControlRecord;
                     indexes : file of tIdxRecord; 
                   end;
  tData          = file of tModel;
  tListOcaSpace  = record
                     data    : tData;
                     control : tControl;
                   end;

//(var this : tListOcaSpace);
  procedure newEmptyList (var this : tListOcaSpace; path, filename : string);
  procedure insert       (var this : tListOcaSpace; item : tModel);
  procedure deletePos    (var this : tListOcaSpace; pos : idxRange);
  procedure deleteItem   (var this : tListOcaSpace; item : tModel);

  function  isEmpty      (var this : tListOcaSpace) : Boolean;
  function  length       (var this : tListOcaSpace) : integer;
  function  first        (var this : tListOcaSpace) : idxRange;
  function  last         (var this : tListOcaSpace) : idxRange;
  function  next         (var this : tListOcaSpace; pos : idxRange) : idxRange;
  function  search       (var this : tListOcaSpace; key : tKey; var pos : idxRange) : boolean;

  function  isValidPos   (var this : tListOcaSpace; pos : idxRange) : Boolean;

implementation

function getControlRecord(var this : tListOcaSpace) : tControlRecord;
var 
  Rc : tControlRecord;
begin
  reset(this.control.control);
  seek (this.control.control, 0);
  read (this.control.control, Rc);
  close(this.control.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tListOcaSpace; Rc : tControlRecord);
begin
  reset(this.control.control);
  seek (this.control.control, 0);
  write(this.control.control, Rc);
  close(this.control.control);
end;

procedure newEmptyList (var this : tListOcaSpace; path, filename : string);
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
  assign(this.data, fullFileName + '.ctrl');
  if not fileexists(fullFileName + '.ctrl') then
    begin
      rewrite(this.control.control);
      Rc.first  := NULLIDX;
      Rc.last   := NULLIDX;
      Rc.erased := NULLIDX;
      Rc.count  := 0;
      seek(this.control.control, 0);
      write(this.control.control, Rc);
    end
  else
    reset(this.control.control);
  close(this.control.control);

  assign(this.data, fullFileName + '.idx');
  if not fileexists(fullFileName + '.idx') then
    rewrite(this.control.indexes)
  else
    reset(this.control.indexes);
  close(this.control.indexes)

end;

function  isEmpty (var this : tListOcaSpace) : Boolean;
var
  Rc : tControlRecord;
begin
  Rc      := getControlRecord(this);
  isEmpty := Rc.count = 0;
end;

function  length (var this : tListOcaSpace) : integer;
var
  Rc : tControlRecord;
begin
  Rc     := getControlRecord(this);
  length := Rc.count;
end;

function  first (var this : tListOcaSpace) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc    := getControlRecord(this);
  first := Rc.first;
end;

function  last (var this : tListOcaSpace) : idxRange;
var
  Rc : tControlRecord;
begin
  Rc   := getControlRecord(this);
  last := Rc.last;
end;

function  next (var this : tListOcaSpace; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord
  Ridx : tIdxRecord;
begin
  if pos = NULLIDX then
    begin
      Rc   := getControlRecord(this);
      next := Rc.first;
    end
  else
    begin
      reset(this.control.indexes);
      seek (this.control.indexes, pos);
      next := pos.next;  
    end;  
end;

function  search (var this : tListOcaSpace; key : tKey; var pos : idxRange) : boolean;
var
  found : boolean;
  Rc    : tControlRecord;
  Ridx  : tIdxRecord;  
begin
  found := false;
  Rc    := getControlRecord(this.control.control);
  pos   := NULLIDX;
  if Rc.first <> Rc.last then
    begin
      reset(this.control.indexes);
      seek(this.control.indexes, Rc.first);
      read(this.control.indexes, Ridx);

      if Ridx.key = key then
        found := true
      else
        begin
          pos := 
        end;
  while Ridx.next <> NULLIDX and Ridx.key< key do 
  begin

    seek(this.control.indexes, Ridx.next);
    read(this.control.indexes, Ridx);
  end;

  search := found;
end;

procedure insert (var this : tListOcaSpace; item : tModel);
begin
  
end;

procedure deletePos (var this : tListOcaSpace; pos : idxRange);
begin
  
end;

procedure deleteItem (var this : tListOcaSpace; item : tModel);
begin
  
end;

function  isValidPos (var this : tListOcaSpace; pos : idxRange) : Boolean;
begin
  isValidPos := pos <> NULLIDX;
end;

end.