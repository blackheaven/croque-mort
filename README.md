# croque-mort

Dead simple broken links checker on local HTML folders.

> _Croque-mort_, colloquial french for "undertaker", literally "deceased biter".
> This naming appeared before French Revolution, it was referring to the legend, they were checking the death, biting their big toe.
>
> `croque-mort` is checking for dead (broken) links.

```
$ croque-mort run -u https://preview.difolco.dev/ -d ../../writings/blog/public -v
# Broken links

../../writings/blog/public/2022-12/polysemy/index.html:https://blog.dev/2022-12/polysemy-injection/./2022-12-14_polysemy-effects-injection.md

Run over 1 base URLs and 637 files, only 1 links where broken out of 14432

```
