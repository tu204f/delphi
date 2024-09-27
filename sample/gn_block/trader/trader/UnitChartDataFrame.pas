unit UnitChartDataFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Procs,
  FMXTee.Chart,
  FMX.Layouts,
  Lb.ReadPrice,
  Lb.PositionTrade,
  FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

const
  COUNT_RSI = 14;

type
  TChartDataFrame = class(TFrame)
    Rectangle: TRectangle;
    Chart: TChart;
    Layout: TLayout;
    Memo1: TMemo;
    Series: TLineSeries;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
  private
    FTypeTrande: TTypeTrande;
    FCandels: TCandelList;
  protected
    procedure SetSendTrade(ACandel: TCandel; ATypeTrande: TTypeTrande);
    property Candels: TCandelList read FCandels;
  public
    PositionTrade: TPositionTrade;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetUpCandel(ACandel: TCandel; ATypeTrande: TTypeTrande);
  end;

implementation

{$R *.fmx}

{ TChartDataFrame }

constructor TChartDataFrame.Create(AOwner: TComponent);
begin
  inherited;
  FTypeTrande := TTypeTrande.ttNull;
  FCandels := TCandelList.Create;

  PositionTrade := TPositionTrade.Create;
end;

destructor TChartDataFrame.Destroy;
begin
  FreeAndNil(PositionTrade);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TChartDataFrame.SetUpCandel(ACandel: TCandel; ATypeTrande: TTypeTrande);
begin
  FCandels.Add(ACandel);
  Series.Add(ACandel.Close);
  if FTypeTrande <> ATypeTrande then
  begin
    SetSendTrade(ACandel,FTypeTrande);
    FTypeTrande := ATypeTrande;
  end;
end;

procedure TChartDataFrame.Clear;
begin
  FCandels.Clear;
  Series.Clear;
  Memo1.Lines.Clear;
end;

procedure TChartDataFrame.SetSendTrade(ACandel: TCandel; ATypeTrande: TTypeTrande);
begin
  if RadioButton1.IsChecked then
    if ATypeTrande = TTypeTrande.ttLong then
    begin
      Memo1.Lines.Add('buy ' + ACandel.Close.ToString);
    end;

  if RadioButton2.IsChecked then
    if ATypeTrande = TTypeTrande.ttShort then
    begin
      Memo1.Lines.Add('sell ' + ACandel.Close.ToString);
    end;



end;

end.
