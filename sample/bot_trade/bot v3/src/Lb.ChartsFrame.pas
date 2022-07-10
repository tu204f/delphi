unit Lb.ChartsFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  Lb.Candel.SysUtils,
  Lb.CandelFrame, FMX.Objects;

type
  TCandelFrameList =  TObjectList<TCandelFrame>;

  TCandelsCharFrame = class(TFrame)
    Layout: TLayout;
  public type
    TTypeSC = (tscSearch, tscComing);
    TSCCandel = record
      Candel: TCandel;
      TypeSC: TTypeSC;
    end;
    TSCCandelList = TList<TSCCandel>;
  private
    FCandelFrames: TCandelFrameList;
    FCandels: TSCCandelList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowCandels(const ASearchCandels: TCandelList; const AComingCandels: TCandelList = nil);
  end;

implementation

{$R *.fmx}

{ TCandelsCharFrame }

constructor TCandelsCharFrame.Create(AOwner: TComponent);
begin
  inherited;
  FCandels := TSCCandelList.Create;
  FCandelFrames := TCandelFrameList.Create;
end;

destructor TCandelsCharFrame.Destroy;
begin
  FreeAndNil(FCandelFrames);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TCandelsCharFrame.ShowCandels(const ASearchCandels, AComingCandels: TCandelList);

  procedure _MaxAndMinPrice(var AMax, AMin: Double);
  var
    i, iCount: Integer;
    xSC: TSCCandel;
  begin
    AMax := 0;
    AMin := 0;
    iCount := FCandels.Count;
    if iCount > 0 then
    begin
      xSC := FCandels[0];
      AMax := xSC.Candel.High;
      AMin := xSC.Candel.Low;
      for i := 1 to iCount - 1 do
      begin
        xSC := FCandels[i];
        if AMax < xSC.Candel.High then
          AMax := xSC.Candel.High;
        if AMin > xSC.Candel.Low then
          AMin := xSC.Candel.Low;
      end;
    end;
  end;

  procedure _CreateCandelChart(const AMax, AMin: Double);
  var
    i, iCount: Integer;
    xSC: TSCCandel;
    xCandelFrame: TCandelFrame;
    xDeltaW: Single;
  begin
    iCount := FCandels.Count;
    if iCount > 0 then
    begin
      xDeltaW := Layout.Width/iCount;
      for i := 0 to iCount - 1 do
      begin
        xSC := FCandels[i];
        xCandelFrame := TCandelFrame.Create(nil);
        xCandelFrame.SetBounds(i * xDeltaW, 0, xDeltaW, Layout.Height);
        xCandelFrame.Parent := Layout;
        xCandelFrame.SetShow(xSC.Candel,AMax,AMin,(xSC.TypeSC = TTypeSC.tscComing));
        FCandelFrames.Add(xCandelFrame);
      end;
    end;
  end;

var
  xSC: TSCCandel;
  xMax, xMin: Double;
begin
  FCandels.Clear;
  FCandelFrames.Clear;

  for var xC in ASearchCandels do
  begin
    xSC.Candel := xC;
    xSC.TypeSC := TTypeSC.tscSearch;
    FCandels.Add(xSC);
  end;

  for var xC in AComingCandels do
  begin
    xSC.Candel := xC;
    xSC.TypeSC := TTypeSC.tscComing;
    FCandels.Add(xSC);
  end;

  _MaxAndMinPrice(xMax,xMin);
  _CreateCandelChart(xMax,xMin);
end;

end.
