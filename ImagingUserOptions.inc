{
  Deskew options for Vampyre Imaging Library.
  Included from Imaging/ImagingOptions.inc when IMAGING_USER_OPTIONS is defined
  (project root must be on the include path).
  Imaging/ itself is synced from imaginglib by Scripts/sync_imaging.sh, keep
  Deskew specific settings here.
}

{ Image file formats Deskew does not need }

// Core
{$DEFINE DONT_LINK_DDS}
{$DEFINE DONT_LINK_JNG}
{$DEFINE DONT_LINK_MNG}
{$DEFINE DONT_LINK_RADHDR}

// Ext
{$DEFINE DONT_LINK_JPEG2000}
{$DEFINE DONT_LINK_PCX}
{$DEFINE DONT_LINK_XPM}
{$DEFINE DONT_LINK_PSD}
