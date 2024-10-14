unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.Bybit.InstrumentsInfo,
  Lb.Bybit.RecentTrade;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FBuffers: TStrings;
  public
    BybitRecentTrade: TBybitRecentTrade;
    procedure RecentTradeOnEventEndLoading(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBuffers := TStringList.Create;

  BybitRecentTrade := TBybitRecentTrade.Create;
  BybitRecentTrade.OnEventEndLoading := RecentTradeOnEventEndLoading;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FBuffers);
  FreeAndNil(BybitRecentTrade);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Список сделок
  BybitRecentTrade.Category := TTypeCategory.tcLinear;
  BybitRecentTrade.Symbol := 'BTCUSDT';
  BybitRecentTrade.Limit := 100;
  BybitRecentTrade.Start(1000);
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  BybitRecentTrade.Stop;
end;

procedure TMainForm.RecentTradeOnEventEndLoading(Sender: TObject);

  procedure _SaveTrade(S: String);
  var
    xFS: TFileStream;
    xPath: String;
    xBuffer: TArray<Byte>;
    xLenBuf: Integer;
  begin
    xPath := ExtractFilePath(ParamStr(0)) + 'trad_3.txt';
    xFS := TFileStream.Create(xPath, fmCreate);
    try
      xBuffer := TEncoding.Default.GetBytes(S);
      xLenBuf := Length(xBuffer);
      xFS.Write(xBuffer,xLenBuf);
    finally
      FreeAndNil(xFS);
    end;
  end;

var
  xIndex: Integer;
  xS: String;
  i, iCount: Integer;
  xRecentTrade: TRecentTradeObject;
begin
  //Массив последний сделки
  Memo.Lines.Clear;
  iCount := BybitRecentTrade.RecentTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xRecentTrade := BybitRecentTrade.RecentTrades.Items[i];
      xIndex := FBuffers.IndexOf(xRecentTrade.ExecID);
      if xIndex < 0 then
      begin
        // Сохроняем в буфер
        xS :=
          xRecentTrade.ExecID + ';' +
          xRecentTrade.Symbol + ';' +
          xRecentTrade.Price.ToString + ';' +
          xRecentTrade.Size.ToString + ';' +
          GetStrToTypeSide(xRecentTrade.Side) + ';' +
          xRecentTrade.Time.ToString;
        //_SaveTrade(xS);
        Memo.Lines.Add(xS);
        FBuffers.Add(xRecentTrade.ExecID);
      end
      else
        FBuffers.Delete(xIndex);
    end;
end;

end.
