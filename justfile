secret := `cat .secret`
input DAY:
	curl -H '{{secret}}' https://adventofcode.com/2022/day/{{DAY}}/input
