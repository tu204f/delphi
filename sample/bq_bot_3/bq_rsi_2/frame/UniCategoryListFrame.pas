unit UniCategoryListFrame;

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
  Lb.Category,
  Lb.SysUtils;

type
  ///<summary>Заявка</summary>
  TCategoryListFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
  private
    FManagerCategory: TManagerCategory;
    procedure InitCategoryFrame;
    procedure SetManagerCategory(const Value: TManagerCategory);
    function GetSide: TTypeBuySell;
  protected
    Category1: TCategoryFrame;
    Category2: TCategoryFrame;
    Category3: TCategoryFrame;
    Category4: TCategoryFrame;
    Category5: TCategoryFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Side: TTypeBuySell read GetSide;
    property ManagerCategory: TManagerCategory read FManagerCategory write SetManagerCategory;
  end;

implementation

uses
  Lb.Logger;

{$R *.fmx}

{ TOrderCategoryFrame }

constructor TCategoryListFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitCategoryFrame;
end;

destructor TCategoryListFrame.Destroy;
begin
  FreeAndNil(Category1);
  FreeAndNil(Category2);
  FreeAndNil(Category3);
  FreeAndNil(Category4);
  FreeAndNil(Category5);
  inherited;
end;

procedure TCategoryListFrame.InitCategoryFrame;

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

function TCategoryListFrame.GetSide: TTypeBuySell;
begin
  if Assigned(FManagerCategory) then
    Result := FManagerCategory.Side
  else
    Result := TTypeBuySell.tsNull;
end;

procedure TCategoryListFrame.SetManagerCategory(const Value: TManagerCategory);

  function _IsCriteria(AIndex: Integer): Boolean;
  begin
    Result := (FManagerCategory.Count - 1) >= AIndex;
  end;

begin
  FManagerCategory := Value;
  case FManagerCategory.Side of
    tsBuy: begin
      Category1.Category := FManagerCategory.Items[0];
      Category2.Category := FManagerCategory.Items[1];
      Category3.Category := FManagerCategory.Items[2];
      Category4.Category := FManagerCategory.Items[3];
      Category5.Category := FManagerCategory.Items[4];
    end;
    tsSell: begin
      Category5.Category := FManagerCategory.Items[0];
      Category4.Category := FManagerCategory.Items[1];
      Category3.Category := FManagerCategory.Items[2];
      Category2.Category := FManagerCategory.Items[3];
      Category1.Category := FManagerCategory.Items[4];
    end;
  end;
end;

end.
