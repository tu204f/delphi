unit Lb.StriGrid;

interface

implementation

procedure _SetAddCol(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
var
  xCol: TStringColumn;
begin
  xCol := TStringColumn.Create(AStrGrid);
  xCol.Header := AHeader;
  xCol.Parent := AStrGrid;
  xCol.Width := AWidth;
end;

procedure _SetHeaders(const AGrid: TStringGrid);
begin
  _SetAddCol(AGrid,'ID',50);
  _SetAddCol(AGrid,'Time',120);
  _SetAddCol(AGrid,'Open');
  _SetAddCol(AGrid,'High');
  _SetAddCol(AGrid,'Low');
  _SetAddCol(AGrid,'Close');
  _SetAddCol(AGrid,'Vol');
  _SetAddCol(AGrid,'RSI',125);
  _SetAddCol(AGrid,'Avg.RSI',125);
//    _SetAddCol(AGrid,'ADR',125);
//    _SetAddCol(AGrid,'K%',125);
//    _SetAddCol(AGrid,'D%',125);
end;

end.
