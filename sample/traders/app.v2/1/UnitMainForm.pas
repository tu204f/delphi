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
  Vcl.Grids,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    StrGrid: TStringGrid;
    Timer: TTimer;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Export;

var
  IndexCandel: Integer = 0;
  Candels: TParsingCandels = nil;

  ParamBlocks: TParamBlockList;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Timer.Enabled := True;
  IndexCandel := 1;
  //xCandels.LoadFromFile('d:\work\git\delphi\sample\traders\history\SPFB.ED_220920_231215.csv');
  Candels.LoadFromFile('d:\work\git\delphi\sample\traders\history\SBER_240101_240331.csv');

  ProgressBar1.Min := 0;
  ProgressBar1.Max := Candels.Count;
  ProgressBar1.Position := 0;

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Candels := TParsingCandels.Create;
  ParamBlocks := TParamBlockList.Create;
  StrGrid.Cells[0,0] := 'Индекс';
  StrGrid.Cells[1,0] := 'Delta';
  StrGrid.Cells[2,0] := 'Top';
  StrGrid.Cells[3,0] := 'Bottom';
  StrGrid.Cells[4,0] := 'CoffTop';
  StrGrid.Cells[5,0] := 'CoffBottom';
  StrGrid.Cells[6,0] := 'Buy.TP';
  StrGrid.Cells[7,0] := 'Buy.SL';
  StrGrid.Cells[8,0] := 'Buy.R';
  StrGrid.Cells[9,0] := 'Sell.TP';
  StrGrid.Cells[10,0] := 'Sell.SL';
  StrGrid.Cells[11,0] := 'Sell.R';
  StrGrid.Cells[12,0] := 'Risk';
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(ParamBlocks);
  FreeAndNil(Candels);
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _AddParamBlock(AParam: TParam);
  var
    xBlock: TParamBlock;
  begin
    xBlock := TParamBlock.Create;
    xBlock.ID := ParamBlocks.Count;
    xBlock.SetCountParam(14,7);
    xBlock.UpDataParam(AParam);
    ParamBlocks.Add(xBlock);
  end;

  procedure _UpDataParam(AParam: TParam);
  const
    COFF = 2;
  var
    i, iCount: Integer;
    xBlock: TParamBlock;
  begin
    iCount := ParamBlocks.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xBlock := ParamBlocks[i];
        xBlock.UpDataParam(AParam);
        if xBlock.IsFull then
        begin
          xBlock.SetMaxAndMinValue;
          if (xBlock.Risk > COFF) then
          begin
            SetSaveBlock(xBlock);
          end
          else
          begin
            ParamBlocks.Delete(i);
          end;
          Break;
        end;
      end;
  end;

  procedure _ShowGrid;
  var
    xInd: Integer;
    i, iCount: Integer;
    xBlock: TParamBlock;
  begin
    iCount := ParamBlocks.Count;
    if iCount > 0 then
    begin
      StrGrid.RowCount := iCount + 1;
      for i := 0 to iCount - 1 do
      begin
        xInd := (iCount - 1) - i;
        xBlock := ParamBlocks[xInd];
        StrGrid.Cells[0,i + 1] := xInd.ToString;
        StrGrid.Cells[1,i + 1] := xBlock.Delta.ToString;
        StrGrid.Cells[2,i + 1] := xBlock.Top.ToString;
        StrGrid.Cells[3,i + 1] := xBlock.Bottom.ToString;
        StrGrid.Cells[4,i + 1] := xBlock.CoffTop.ToString;
        StrGrid.Cells[5,i + 1] := xBlock.CoffBottom.ToString;
        StrGrid.Cells[6,i + 1] := xBlock.BuyTP.ToString;
        StrGrid.Cells[7,i + 1] := xBlock.BuySL.ToString;
        StrGrid.Cells[8,i + 1] := xBlock.BuyR.ToString;
        StrGrid.Cells[9,i + 1] := xBlock.SellTP.ToString;
        StrGrid.Cells[10,i + 1] := xBlock.SellSL.ToString;
        StrGrid.Cells[11,i + 1] := xBlock.SellR.ToString;
        StrGrid.Cells[12,i + 1] := xBlock.Risk.ToString;
      end;
    end;
  end;

var
  xParam: TParam;
begin
  try
    xParam := Candels.Params[IndexCandel];
    _AddParamBlock(xParam);
    _UpDataParam(xParam);
    _ShowGrid;
    ProgressBar1.Position := IndexCandel;
    Inc(IndexCandel);
    if IndexCandel >= Candels.Count then
    begin
      Label1.Caption := 'стоп';
      Timer.Enabled := False;
    end;
  except
    Timer.Enabled := False;
    raise Exception.Create('Error Message: Последовательная ошибка');
  end
end;

end.
