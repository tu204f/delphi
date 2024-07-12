unit UnitOrderCategoryFrame;

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
  UnitCategoryFrame,
  Lb.Bybit.SysUtils;

type
  ///<summary>Заявка</summary>
  TOrderCategoryFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
  private
    procedure InitCategoryFrame;
  protected
    Category1: TCategoryFrame;
    Category2: TCategoryFrame;
    Category3: TCategoryFrame;
    Category4: TCategoryFrame;
    Category5: TCategoryFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadParam;
    procedure SaveParam;
  end;

implementation

{$R *.fmx}

{ TOrderCategoryFrame }

constructor TOrderCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitCategoryFrame;
end;

destructor TOrderCategoryFrame.Destroy;
begin
  FreeAndNil(Category1);
  FreeAndNil(Category2);
  FreeAndNil(Category3);
  FreeAndNil(Category4);
  FreeAndNil(Category5);
  inherited;
end;

procedure TOrderCategoryFrame.InitCategoryFrame;

  function _InitCategoryFrame(const ALayout: TLayout): TCategoryFrame;
  var
    xFrame: TCategoryFrame;
  begin
    xFrame := TCategoryFrame.Create(nil);
    xFrame.Parent := ALayout;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;

begin
  Category1 := _InitCategoryFrame(Layout1);
  Category2 := _InitCategoryFrame(Layout2);
  Category3 := _InitCategoryFrame(Layout3);
  Category4 := _InitCategoryFrame(Layout4);
  Category5 := _InitCategoryFrame(Layout5);
end;

procedure TOrderCategoryFrame.LoadParam;
begin

end;

procedure TOrderCategoryFrame.SaveParam;
begin

end;

end.
