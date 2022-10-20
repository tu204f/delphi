unit UnitDoubleBarsFrame;

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
  FMX.Layouts,
  UnitBarsFrame;

type
  TDoubleBarsFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
    LayoutSource: TLayout;
    LayoutHistory: TLayout;
  private
    FBarsSource: TBarsFrame;
    FBarsHistory: TBarsFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>Источние данных</summary>
    property BarsSource: TBarsFrame read FBarsSource;
    ///<summary>История на основание, которой принимается решение</summary>
    property BarsHistory: TBarsFrame read FBarsHistory;
  end;

implementation

{$R *.fmx}

{ TDoubleBarsFrame }

constructor TDoubleBarsFrame.Create(AOwner: TComponent);
begin
  inherited;
  // ---------------------------------------
  FBarsSource := TBarsFrame.Create(nil);
  FBarsSource.Parent := LayoutSource;
  FBarsSource.Align := TAlignLayout.Client;
  // ---------------------------------------
  FBarsHistory := TBarsFrame.Create(nil);
  FBarsHistory.Parent := LayoutHistory;
  FBarsHistory.Align := TAlignLayout.Client;
end;

destructor TDoubleBarsFrame.Destroy;
begin
  FreeAndNil(FBarsHistory);
  FreeAndNil(FBarsSource);
  inherited;
end;

end.
