unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids;

type
  TConnectQuikForm = class(TForm)
    LabelStatus: TLabel;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    ButtonSource1: TButton;
    StrGrid: TStringGrid;
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConnectQuikForm: TConnectQuikForm;

implementation

{$R *.dfm}

procedure TConnectQuikForm.Timer1Timer(Sender: TObject);
begin
  if GetIsQuikTable then
  begin
    // контролируем позицию  по бумагам
  end
  else
    SetInitializationTable;
end;

procedure TConnectQuikForm.Button2Click(Sender: TObject);
begin
  var xCaption := 'cap_sr';
  var xStr := TStringList.Create;
  try
    var xS := GetResourceScritpQPL(xCaption,'SRM2','SPBFUT','MA',1,100);
    xStr.Text := xS;
    xStr.SaveToFile(xCaption + '.qpl')
  finally
    FreeAndNil(xStr);
  end;
end;

var
  IndexFile: Integer = 0;

procedure SetSaveFile(AQuikTable: TQuikTable);
var
  xStr: TStrings;
begin
  Inc(IndexFile);
  var S := 'book_file_' + IntToStr(IndexFile) + '.bk';
  xStr := TStringList.Create;
  try
    xStr.Add(FormatDateTime('yyyymmdd hhnnsszzz',Date + Time));
    for var j := 0 to AQuikTable.RowCount - 1 do
    begin
      var xTmpS := '';
      for var i := 0 to AQuikTable.ColCount - 1 do
        xTmpS := xTmpS + AQuikTable.Cells[i,j].AsString + ';';
      xStr.Add(xTmpS);
    end;
    var xPath := ExtractFilePath(ParamStr(0)) + 'book' + '/';
    xStr.SaveToFile(xPath + S);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TConnectQuikForm.EventAddValueBlock(Sender: TObject;
  APointCells: TPointCells; AQuikTable: TQuikTable);
begin
  if SameText(AQuikTable.Name,'book_srm2') then
  begin
    SetSaveFile(AQuikTable);


    StrGrid.ColCount := AQuikTable.ColCount;
    StrGrid.RowCount := AQuikTable.RowCount;

    for var i := 0 to AQuikTable.ColCount - 1 do
      for var j := 0 to AQuikTable.RowCount - 1 do
        StrGrid.Cells[i,j] := AQuikTable.Cells[i,j].AsString;

  end;
  LabelStatus.Caption := 'Состоние загрузка: ' + APointCells.ToString + ' :: ' + AQuikTable.Name;
end;

end.
