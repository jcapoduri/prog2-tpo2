unit oca.space.list;

interface

uses 
  oca.space;

const
  NULLIDX  = -1;

type
  idxRange = NULLIDX..MAXINT
  tControl = record
               first  : idxRange;
               last   : idxRange;
               erased : idxRange;
               count  : Integer;              
             end;
  tData    = array[idxRange] or tOcaSpace;
  tListOcaSpace = record
                    data    : tData;
                    control : tControl;
                  end;

implementation

end.