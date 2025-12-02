unit Lider.CG.Com.GetGoogleZoom;

interface

// latMin, lonMin, LatMax, lonMax deðerleri sormak istesiðiniz polygon ve polyline objesine ait extension deðerleri
// MapPikselWidth, MapPikselHeight deðerleri haritayý çizdiðiniz pencerenin width, height piksel deðerleri // Google.Map.Width,Google.Map.Height

function CalcZoomFactor(latMin, lonMin, LatMax, lonMax: double; MapPikselWidth,
  MapPikselHeight: integer): integer;

implementation

uses
  Windows,
  Math;

const
  EARTH_RADIUS = 6378137;
  EQUATOR_CIRCUMFERENCE = 2 * pi * EARTH_RADIUS;
  INITIAL_RESOLUTION = EQUATOR_CIRCUMFERENCE / 256.0;
  ORIGIN_SHIFT = EQUATOR_CIRCUMFERENCE / 2.0;
  ZOOM_LEVELS = 21;

function _latlontopixels(lat, lon, _zoom: double): TPoint;
var
  mx, my, res: double;
  z: double;
begin
  try
    z := _zoom;

    mx := (lon * ORIGIN_SHIFT) / 180.0;
    my := LN(tan((90 + lat) * pi / 360.0)) / (pi / 180.0);
    my := (my * ORIGIN_SHIFT) / 180.0;
    res := INITIAL_RESOLUTION / power(2, z);
    Result.x := round((mx + ORIGIN_SHIFT) / res);
    Result.y := round((my + ORIGIN_SHIFT) / res);
  except
  end;
end;

function CalcZoomFactor(latMin, lonMin, LatMax, lonMax: double; MapPikselWidth,
  MapPikselHeight: integer): integer;
var
  z, i: integer;
  top_right_pixel, bottom_left_pixel: TPoint;
  _pixels: integer;
  PixelTileSize: integer;
  pixel_range: array[0..ZOOM_LEVELS] of integer;
begin
  Result := 0;

  PixelTileSize := 256 * 1;
  _pixels := PixelTileSize;
  for i := 0 to ZOOM_LEVELS do
  begin
    pixel_range[i] := (_pixels);
    _pixels := _pixels * 2;
  end;

  for z := 0 to ZOOM_LEVELS do
  begin

    bottom_left_pixel := _latlontopixels(latMin, lonMin, z);
    top_right_pixel := _latlontopixels(latMax, lonMax, z);

    if bottom_left_pixel.x > top_right_pixel.x then
      bottom_left_pixel.x := bottom_left_pixel.x - pixel_range[z];

    if (abs(top_right_pixel.x - bottom_left_pixel.x) <= MapPikselWidth) and (abs
      (top_right_pixel.y - bottom_left_pixel.y) <= MapPikselHeight) then
    begin
      Result := z;
    end;
  end;
end;

end.


