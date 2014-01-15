//
// Unit tests for block-chain checkpoints
//
#include <boost/assign/list_of.hpp> // for 'map_list_of()'
#include <boost/test/unit_test.hpp>
#include <boost/foreach.hpp>

#include "../checkpoints.h"
#include "../util.h"

using namespace std;

BOOST_AUTO_TEST_SUITE(Checkpoints_tests)

BOOST_AUTO_TEST_CASE(sanity)
{
    uint256 p1500 = uint256("0xbfc8e3e9a051b6ecea012701ac7dbd5d9eff62469ad39a92767a6853a49983d4");
    uint256 p56000 = uint256("0xb6b54c1dd6ebf853ea6fdc82e5a696ec4ba5b886b1a0aeecc7517cb47e550849");
    BOOST_CHECK(Checkpoints::CheckBlock(1500, p1500));
    BOOST_CHECK(Checkpoints::CheckBlock(56000, p56000));

    
    // Wrong hashes at checkpoints should fail:
    BOOST_CHECK(!Checkpoints::CheckBlock(1500, p56000));
    BOOST_CHECK(!Checkpoints::CheckBlock(56000, p1500));

    // ... but any hash not at a checkpoint should succeed:
    BOOST_CHECK(Checkpoints::CheckBlock(1500+1, p56000));
    BOOST_CHECK(Checkpoints::CheckBlock(56000+1, p1500));

    BOOST_CHECK(Checkpoints::GetTotalBlocksEstimate() >= 56000);
}    

BOOST_AUTO_TEST_SUITE_END()
