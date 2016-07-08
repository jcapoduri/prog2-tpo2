unit oca.space.list;

interface

uses
  sysutils;

const
  NULLIDX  = -1;

type
  idxRange       = NULLIDX..MAXINT;
  tKey           = integer;
  tOcaSpace      = record
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
  tData          = file of tOcaSpace;
  tListOcaSpace  = record
                     data    : tData;
                     control : tControl;
                   end;

//(var this : tListOcaSpace);
  procedure newEmptyList (var this : tListOcaSpace; path, filename : string);
  procedure insert       (var this : tListOcaSpace; item : tOcaSpace);
  procedure deletePos    (var this : tListOcaSpace; pos : idxRange);
  procedure deleteItem   (var this : tListOcaSpace; item : tOcaSpace);

  function  get          (var this : tListOcaSpace; pos : idxRange) : tOcaSpace;
  function  isEmpty      (var this : tListOcaSpace) : boolean;
  function  length       (var this : tListOcaSpace) : integer;
  function  first        (var this : tListOcaSpace) : idxRange;
  function  last         (var this : tListOcaSpace) : idxRange;
  function  next         (var this : tListOcaSpace; pos : idxRange) : idxRange;
  function  search       (var this : tListOcaSpace; key : tKey; var pos : idxRange) : boolean;

  function  isValidPos   (var this : tListOcaSpace; pos : idxRange) : boolean;

implementation

function getControlRecord(var this : tListOcaSpace) : tControlRecord;
var 
  Rc : tControlRecord;
begin
  reset(this.control);
  seek (this.control, 0);
  read (this.control, Rc);
  close(this.control);
  getControlRecord := Rc;
end;

procedure setControlRecord(var this : tListOcaSpace; Rc : tControlRecord);
begin
  reset(this.control);
  seek (this.control, 0);
  write(this.control, Rc);
  close(this.control);
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

function  get (var this : tListOcaSpace; pos : idxRange) : tOcaSpace;
var
  item: tOcaSpace;
begin
  reset (this.data);
  seek  (this.data, pos);
  read  (this.data, item);
  get := item;
end;

procedure update (var this : tListOcaSpace; pos : idxRange; var item : tOcaSpace);
begin
  reset (this.data);
  seek  (this.data, pos);
  write (this.data, item);
  close (this.data);
end;

procedure append (var this : tListOcaSpace; var item : tOcaSpace);
var
  Rc      : tControlRecord;
  pos     : idxRange;
  auxItem : tOcaSpace;
begin
  Rc := getControlRecord(this);
  if Rc.erased = NULLIDX then
    begin
      //item := get(this, )
    end
  else
    begin
      pos       := Rc.erased;
      auxItem   := get(this, Rc.erased);
      Rc.erased := auxItem.next;

      update(this, pos, item);      

      setControlRecord(this, Rc);
    end;

end;

function  next (var this : tListOcaSpace; pos : idxRange) : idxRange;
var
  Rc   : tControlRecord;
  Ridx : idxRange;
  item : tOcaSpace;
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

function  search (var this : tListOcaSpace; key : tKey; var pos : idxRange) : boolean;
var
  found : boolean;
  Rc    : tControlRecord;
  Ridx  : idxRange; 
  item  : tOcaSpace;
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
      until found or Ridx = Rc.last or item.number > key;
    end;
  
  if found then pos := Ridx;

  search := found;
end;

procedure insert (var this : tListOcaSpace; item : tOcaSpace);
var
  pos, auxPos : idxRange;
  auxItem     : tOcaSpace;
  Rc          : tControlRecord
begin
  if not search(this, item, pos) then
    begin      
      auxPos  := append(this, item);
      Rc      := getControlRecord(this);

      if pos = NULLIDX then
        begin          
          Rc.first := auxPos;
          setControlRecord(this, Rc);
        end;

      if pos = Rc.last then
        begin          
          Rc.last := auxPos;
          setControlRecord(this, Rc);
        end;

      auxItem := get(this, pos);
      pos     := auxItem.next;

      auxItem.next := auxPos;
      update(this, auxItem);

      item.next = pos;
      update(this, item);      
    end;
end;

procedure deletePos (var this : tListOcaSpace; pos : idxRange);
begin
  
end;

procedure deleteItem (var this : tListOcaSpace; item : tOcaSpace);
begin
  
end;

function  isValidPos (var this : tListOcaSpace; pos : idxRange) : Boolean;
begin
  isValidPos := pos <> NULLIDX;
end;

end.